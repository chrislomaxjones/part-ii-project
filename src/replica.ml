(* replica.ml *)

open Types
open Core
open Core.Fn    
open Lwt.Infix
open Log.Logger

(* Size of reconfiguration window *)
let window = 5

(* Type of replicas *)
type t = {
  (* Id of the replica node *)
  id : replica_id;
  
  (* The state of the application it is replicating *)
  mutable app_state : app_state;
  
  (* The slot number for the next empty slot to which the replica
     will propose the next command *)
  mutable slot_in : slot_number;
  
  (* The slot number for the next slot in which a decision needs to be
     made about which command to commit to that slot before the 
     application state can be updated *)
  mutable slot_out : slot_number;
  
  (* Set of request commands sent to the replica *)
  mutable requests : command list;
  
  (* Set of commands proposed by the replica *)
  mutable proposals : proposal list;
  
  (* Set of commands upon which a slot has been decided *)
  mutable decisions : proposal list;
  
  (* Set of leader ids that the replica has in its current configuration *)
  mutable leaders : Uri.t list;
}

(* Function new_replica returns a new replica given a list of leader ids *)
let initialize leader_ids = {
  id = create_id ();
  app_state = initial_state;
  slot_in   = 1;
  slot_out  = 1;
  requests  = [];
  proposals = [];
  decisions = [];
  leaders   = leader_ids;
}

(* Simple function that maps from requests to results *)
(* Actual replica implementations will have to propose these
   for sequence slots etc.
   But for now this is just to simulate message passing capability
*)
let receive_request (replica : t) (cmd : command)  : unit =
  Lwt.ignore_result (
    write_with_timestamp INFO (
      "Receive client request, command " ^ (string_of_command cmd) ));
  replica.requests <- List.append (replica.requests) [cmd]

(* TODO: Implement configurations *)
(* We won't yet worry about reconfigurations *)
let isreconfig op = false;;

(* Perform the command c on the application state of replica *)
let perform replica c =
  let slot_out = replica.slot_out in
  let ((id, uri), cid, op) = c in
  let decisions_inv = List.Assoc.inverse replica.decisions in
  let is_lower_slot = (match List.Assoc.find decisions_inv ~equal:commands_equal c with
  | None -> false
  | Some s -> s < slot_out) in
    if is_lower_slot || isreconfig op  then
      replica.slot_out <- replica.slot_out + 1
    else 
      (* ATOMIC - May need to make this an atomic execution block *)
      (* Update application state *)
      Lwt.ignore_result(
        write_to_log TRACE (string_of_command c));

      let next_state, results = apply replica.app_state op in
        replica.app_state <- next_state;

        Lwt.ignore_result(
          write_to_log TRACE (string_of_state replica.app_state));    
        Lwt.ignore_result (
          write_with_timestamp INFO ("Update application state to "
                                     ^ string_of_state next_state));

        replica.slot_out <- replica.slot_out + 1;
        (* END ATOMIC *)

        Lwt.ignore_result (
          write_with_timestamp INFO ("Send response message (" ^
                                    (string_of_int cid) ^ ", " ^
                                    (string_of_result results) ^ ") to " ^
                                    (Uri.to_string uri)));

      (* Send a response message to the client *)
      Message.send_request (Message.ClientResponseMessage(cid,results)) uri |>
      Lwt.ignore_result

let rec try_execute (replica : t) (p : proposal) =
  let (s,c) = p in
  if (List.mem replica.requests c ~equal:commands_equal) &&
     (replica.slot_in = s) then
    Lwt.ignore_result (write_with_timestamp INFO "this command is already in the requests")
  else
    Lwt.ignore_result (write_with_timestamp INFO "this command is not already in the requests");
  
    (* Find a decision corresponding to <slot_out, _>
       Such decisions are possibly ready to have their commands applied
       (given there are no other commands proposed for slot_out) *)
    let slot_out = replica.slot_out in 
    match List.Assoc.find replica.decisions ~equal:(=) slot_out with
    | None -> ()
    | Some c' ->  (* A command c' is possibly ready *)
      (* Check next if there is another command c'' that this replica has
         proposed for slot_out. *)
      (match List.Assoc.find replica.proposals ~equal:(=) slot_out with
       | None -> ()
       | Some c'' -> 
        (* Remove the proposal <slot_out, c''> from set of proposals *)
        let new_proposals = List.filter replica.proposals 
            ~f:(compose not (proposals_equal (slot_out, c'')) ) in        
        replica.proposals <- new_proposals;
        
        (* If c' = c'' then we don't need to re-propose this command,
           since we're about to commit <slot_out, c'> anyway.
      
           However, if c' =/= c'' then we add c'' back into the set of
           requests so that it can be proposed again at a later time
           by this replica *)
        if not (commands_equal c' c'') then
          replica.requests <- List.append (replica.requests) [c'']
        else ());
    (* Perform the operation of command c' on the application state *)
    perform replica c';
    try_execute replica p

(* ... *)
let receive_decision (replica : t) (p : proposal ) : unit =
  Lwt.ignore_result (
    write_with_timestamp INFO (
      "Receive decision for " ^(string_of_proposal p) ^
      ", slot_out = " ^ (string_of_int replica.slot_out) ^ 
      ", slot_in = " ^ (string_of_int replica.slot_in)));

  (* Append the received proposal to the set of decisions *)
  replica.decisions <- List.append (replica.decisions) [p];

  (* Try to execute commands ... *)
  try_execute replica p

let do_proposal (c : command) (replica : t) = 
  write_with_timestamp INFO 
    ("There is no command yet committed to slot " ^ (string_of_int replica.slot_in)) >>= fun () ->

  (* Remove c from the list of requests *) 
  let new_requests = List.filter replica.requests ~f:(compose not (commands_equal c)) in
      
    (* Set the replica to include these updated lists *)
    replica.requests <- new_requests;

    (* Add <slot_in,c> to the list of proposals *)
    replica.proposals <- (replica.slot_in, c) :: replica.proposals;
    
    write_with_timestamp INFO
      ("Propose " ^ (Types.string_of_proposal (replica.slot_in,c))) >>= fun () ->

    (* Finally broadcast a message to all of the leaders notifying them of proposal *)
    Lwt_list.iter_p (fun uri ->
      let msg = Message.ProposalMessage (replica.slot_in, c) in  
        Message.send_request msg uri) replica.leaders

(* Function attempts to take one request and propose it.

   A command can be proposed if its in the set of requests and there is a
   slot free in the configuration window.

   The command may also be a re-config command, in which case a reconfig
   is performed in which a new set of leaders is introduced.
*)
let propose replica =
  let slot_in, slot_out = replica.slot_in, replica.slot_out in
  if (slot_in < slot_out + window) && (not (List.is_empty replica.requests)) then
    let c = List.hd_exn replica.requests in
    
    (* See if there is a command in the window that is a reconfig *)
    (* If there is then perform a reconfiguration *)
    (* TODO: ADD SUPPORT FOR RECONFIGURATIONS *)
    (* ... *)
    
    (* See if there exists a command currently committed for slot_in *)
    match List.Assoc.find replica.decisions ~equal:(=) slot_in with
     | None -> do_proposal c replica   
     | Some c' when commands_equal c c' -> do_proposal c replica
     | Some _ -> Lwt.return_unit >>= fun () ->

    (* Increment slot_in on the replica *)
    Lwt.return (replica.slot_in <- replica.slot_in + 1) >>= fun () -> 

    write_with_timestamp INFO ("Increment slot_in to " ^ (string_of_int replica.slot_in))
  else Lwt.return_unit

(* Repeatedly call the propose function:
   This is so that as requests arrive we can attempt to propose them *)
let rec propose_rec replica =
  propose replica >>= 
  fun () -> Lwt_unix.sleep 0.00001 >>=
  fun () -> propose_rec replica

(* Starts a server that will run the service
   This is mostly Capnproto boilerplate *)
let start_server replica host port =
  Message.start_new_server 
    ~request_callback:(receive_request replica) 
    ~proposal_callback:(receive_decision replica) 
    host port

let print_uri uri =
  Lwt_io.printl ("Spinning up a replica with URI: " ^ (Uri.to_string uri))

(* Initialize a new replica and its lwt threads *)
let new_replica host port leader_uris =  
  let replica = initialize leader_uris in
    start_server replica host port >>=
    fun uri -> print_uri uri >>=
    fun () -> propose_rec replica
