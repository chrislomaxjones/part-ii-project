(* message.ml *)

open Types
open Capnp_rpc_lwt
open Lwt.Infix

exception DeserializationError
let sturdy_refs = Hashtbl.create 10


  

let serialize_phase1_response acceptor_id ballot_num accepted : Yojson.Basic.json =
  let acceptor_id = ("acceptor_id", `String (Types.string_of_id acceptor_id)) in
  let ballot_json = Yojson.Basic.Util.to_assoc (Ballot.serialize ballot_num) in
  let pvalues_json = Yojson.Basic.Util.to_assoc ( (`Assoc [("pvalues", Pval.serialize_list accepted)])) in
  let response_json = `Assoc ( acceptor_id :: (Core.List.concat [ballot_json; pvalues_json]) ) in  
  `Assoc [ ("response", response_json )]

let deserialize_phase1_response (response_json : Yojson.Basic.json) =
  let inner_json = Yojson.Basic.Util.member "response" response_json in
  let acceptor_id_json = Yojson.Basic.Util.member "acceptor_id" inner_json in
  let ballot_number_json = Yojson.Basic.Util.member "ballot_num" inner_json in  
  let pvalues_json = Yojson.Basic.Util.member "pvalues" inner_json in
  (acceptor_id_json |> Yojson.Basic.Util.to_string |> Types.id_of_string,
   Ballot.deserialize (`Assoc [("ballot_num",ballot_number_json)]),
   Pval.deserialize_list pvalues_json)

let serialize_phase2_response acceptor_id ballot_num : Yojson.Basic.json =
  let acceptor_id = ("acceptor_id", `String (Types.string_of_id acceptor_id)) in  
  let ballot_json = Yojson.Basic.Util.to_assoc (Ballot.serialize ballot_num) in
  `Assoc [ ("response", `Assoc ( acceptor_id :: ballot_json ) ) ]

let deserialize_phase2_response (response_json : Yojson.Basic.json) =
  let inner_json = Yojson.Basic.Util.member "response" response_json in
  let acceptor_id_json = Yojson.Basic.Util.member "acceptor_id" inner_json in
  let ballot_number_json = Yojson.Basic.Util.member "ballot_num" inner_json in  
  (acceptor_id_json |> Yojson.Basic.Util.to_string |> Types.id_of_string,
   Ballot.deserialize (`Assoc [ ("ballot_num", ballot_number_json) ]))
















(* Exceptions resulting in undefined values being sent in Capnp unions *)
exception Undefined_oper;;
exception Undefined_result;;

(* Exception arising from the wrong kind of response being received *)
exception Invalid_response;;

(* Expose the API service for the RPC system *)
module Api = Message_api.MakeRPC(Capnp_rpc_lwt);;

let local ?(request_callback : (command -> unit) option) 
          ?(proposal_callback : (proposal -> unit) option)
          ?(response_callback : ((command_id * result) -> unit) option)
          ?(phase1_callback : (Ballot.t -> (Types.unique_id * Ballot.t * Pval.t list)) option)
          ?(phase2_callback : (Pval.t -> (Types.unique_id * Ballot.t)) option)
          () =

  let module Message = Api.Service.Message in
  Message.local @@ object
    inherit Message.service
  
    method phase2_impl params release_param_caps =
      let open Message.Phase2 in
      let module Params = Message.Phase2.Params in
      let pvalue = Params.pvalue_get params
                   |> Yojson.Basic.from_string
                   |> Pval.deserialize in
      release_param_caps ();
      match phase2_callback with Some f ->
      let (acceptor_id, ballot_num) = f(pvalue) in
      let json = serialize_phase2_response acceptor_id ballot_num in
      let result_str = Yojson.Basic.to_string json in
      let response,results = Service.Response.create Results.init_pointer in
      Results.result_set results result_str;
      Service.return response;

    method phase1_impl params release_param_caps =
      let open Message.Phase1 in
      let module Params = Message.Phase1.Params in
      let ballot_number = Params.ballot_number_get params 
                              |> Yojson.Basic.from_string 
                              |> Ballot.deserialize in
      release_param_caps ();
      match phase1_callback with Some f ->
      let (acceptor_id, ballot_num', pvalues)  = f(ballot_number) in
      let json = serialize_phase1_response acceptor_id ballot_num' pvalues in
      let result_str = Yojson.Basic.to_string json in
      let response, results = Service.Response.create Results.init_pointer in
      Results.result_set results result_str;
      Service.return response;

    method client_response_impl params release_param_caps =
      let open Message.ClientResponse in
      let module Params = Message.ClientResponse.Params in
    
      let open Api.Reader.Message in
      
      let result_reader = Params.result_get params in
      
      (* Pull out all the necessary data from the params *)
      let result = (match Result.get result_reader with
      | Result.Failure -> Types.Failure
      | Result.Success -> Types.Success
      | Result.Read v  -> Types.ReadSuccess v
      | Result.Undefined _ -> raise Undefined_result) in
      
      let command_id = Params.command_id_get params in

      (* Call a callback to notify client *)
      match response_callback with Some h -> h(command_id,result);

      (* Release capabilities, doesn't matter for us *)
      release_param_caps ();

      (* Return an empty response *)
      Service.return_empty ();

    method send_proposal_impl params release_param_caps =
      let open Message.SendProposal in
      let module Params = Message.SendProposal.Params in

      (* Pull out all the slot number from params *)
      let slot_number = Params.slot_number_get params in
    
      (* Get an API reader for the command, since its a nested struct *)
      let cmd_reader  = Params.command_get params in
      
      (* Retrieve the fields from the command struct passed in decision *)
      let open Api.Reader.Message in
    
      (* Retrieve the client id and command id fields from the struct *)
      let id = Command.client_id_get cmd_reader in
      let uri = Command.client_uri_get cmd_reader in 
      let command_id = Command.command_id_get cmd_reader in
        
      (* Operation is more difficult as it is a nested struct *)
      let op_reader = Command.operation_get cmd_reader in
      
      (* Operations are a union type in Capnp so match over the variant *)
      let operation = (match Command.Operation.get op_reader with
      | Command.Operation.Nop -> Types.Nop
      | Command.Operation.Create c_struct ->
        let k = Command.Operation.Create.key_get c_struct in
        let v = Command.Operation.Create.value_get c_struct in
        Types.Create(k,v)
      | Command.Operation.Read r_struct -> 
        let k = Command.Operation.Read.key_get r_struct in
        Types.Read(k)
      | Command.Operation.Update u_struct ->
        let k = Command.Operation.Update.key_get u_struct in
        let v = Command.Operation.Update.value_get u_struct in
        Types.Update(k,v)
      | Command.Operation.Remove r_struct ->
        let k = Command.Operation.Remove.key_get r_struct in
        Types.Remove(k)
      | Command.Operation.Undefined(_) -> raise Undefined_oper) in
      
      (* Form the proposal from the message parameters *)
      let proposal = (slot_number, ((Core.Uuid.of_string id,Uri.of_string uri), command_id, operation)) in
      
      (* Do something with the proposal here *)
      (* This is nonsense at the moment *)
      (match proposal_callback with
      | None -> ()
      | Some g -> g(proposal) );
      
      (* Release capabilities, doesn't matter for us *)
      release_param_caps ();

      (* Return an empty response *)
      Service.return_empty ();

    method decision_impl params release_param_caps = 
      let open Message.Decision in
      let module Params = Message.Decision.Params in
    
      (* Get slot number *)
      let slot_number = Params.slot_number_get params in

      (* Get an API reader for the command, since its a nested struct *)
      let cmd_reader  = Params.command_get params in
      
      (* Retrieve the fields from the command struct passed in decision *)
      let open Api.Reader.Message in
    
      (* Retrieve the client id and command id fields from the struct *)
      let id = Command.client_id_get cmd_reader in
      let uri = Command.client_uri_get cmd_reader in
      let command_id = Command.command_id_get cmd_reader in
        
      (* Operation is more difficult as it is a nested struct *)
      let op_reader = Command.operation_get cmd_reader in
      
      (* Operations are a union type in Capnp so match over the variant *)
      let operation = (match Command.Operation.get op_reader with
      | Command.Operation.Nop -> Types.Nop
      | Command.Operation.Create c_struct ->
        let k = Command.Operation.Create.key_get c_struct in
        let v = Command.Operation.Create.value_get c_struct in
        Types.Create(k,v)
      | Command.Operation.Read r_struct -> 
        let k = Command.Operation.Read.key_get r_struct in
        Types.Read(k)
      | Command.Operation.Update u_struct ->
        let k = Command.Operation.Update.key_get u_struct in
        let v = Command.Operation.Update.value_get u_struct in
        Types.Update(k,v)
      | Command.Operation.Remove r_struct ->
        let k = Command.Operation.Remove.key_get r_struct in
        Types.Remove(k)
      | Command.Operation.Undefined(_) -> raise Undefined_oper) in
      
      (* Form the proposal from the message parameters *)
      let proposal = (slot_number, ((Core.Uuid.of_string id,Uri.of_string uri), command_id, operation)) in
      
      (* Call the callback function that will process the decision *)
      (match proposal_callback with
      | None -> ()
      | Some g -> g(proposal) );
 
      (* Release capabilities, doesn't matter for us *)
      release_param_caps ();

      (* Return an empty response *)
      Service.return_empty ();

    method client_request_impl params release_param_caps =
      let open Message.ClientRequest in
      let module Params = Message.ClientRequest.Params in
      (* Retrieve the fields from the command struct passed in request *)
      let cmd_reader = Params.command_get params in
        let open Api.Reader.Message in
        
        (* Retrieve the client id and command id fields from the struct *)
        let id = Command.client_id_get cmd_reader in
        let uri = Command.client_uri_get cmd_reader in
        let command_id = Command.command_id_get cmd_reader in
        
        (* Operation is more difficult as it is a nested struct *)
        let op_reader = Command.operation_get cmd_reader in
        (* Operations are a union type in Capnp so match over the variant *)
        let operation = (match Command.Operation.get op_reader with
          | Command.Operation.Nop -> Types.Nop
          | Command.Operation.Create c_struct ->
              let k = Command.Operation.Create.key_get c_struct in
              let v = Command.Operation.Create.value_get c_struct in
              Types.Create(k,v)
          | Command.Operation.Read r_struct -> 
              let k = Command.Operation.Read.key_get r_struct in
              Types.Read(k)
          | Command.Operation.Update u_struct ->
              let k = Command.Operation.Update.key_get u_struct in
              let v = Command.Operation.Update.value_get u_struct in
              Types.Update(k,v)
          | Command.Operation.Remove r_struct ->
              let k = Command.Operation.Remove.key_get r_struct in
              Types.Remove(k)
          | Command.Operation.Undefined(_) -> raise Undefined_oper) in

        (* Get back response for request *)
        (* Note here there is a temporay Nop passed *)
        (* This pattern matching is not exhaustive but
           we always want some callback f here 

           So it is suitable to raise an exception
           if one is not passed in this case
        *) 
        match request_callback with Some f ->
        f ((Core.Uuid.of_string id,Uri.of_string uri), command_id, operation);
      
        (* Releases capabilities, doesn't matter for us *)
        release_param_caps ();

        (* Return an empty response *)        
        Service.return_empty ()
  end;;

(*---------------------------------------------------------------------------*)

let client_request_rpc t (cmd : Types.command) =
  let open Api.Client.Message.ClientRequest in
  let request, params = Capability.Request.create Params.init_pointer in
  let open Api.Builder.Message in
    (* Create an empty command type as recognised by Capnp *)
    let cmd_rpc = (Command.init_root ()) in
    
    (* Construct a command struct for Capnp from the cmd argument given *)
    let ((id,uri), command_id, operation) = cmd in
      Command.client_id_set cmd_rpc (Core.Uuid.to_string id);
      Command.client_uri_set cmd_rpc (Uri.to_string uri);
      Command.command_id_set_exn cmd_rpc command_id;
      
      (* Construct an operation struct here *)
      let oper_rpc = (Command.Operation.init_root ()) in
      
      (* Populate the operation struct with the correct values *)
      (match operation with
      | Nop         -> 
        Command.Operation.nop_set oper_rpc
      | Create(k,v) ->
        let create = (Command.Operation.create_init oper_rpc) in
        Command.Operation.Create.key_set_exn create k;
        Command.Operation.Create.value_set create v;
      | Read  (k)   -> 
        let read = Command.Operation.read_init oper_rpc in
        Command.Operation.Read.key_set_exn read k;
      | Update(k,v) ->
        let update = Command.Operation.update_init oper_rpc in
        Command.Operation.Update.key_set_exn update k;
        Command.Operation.Update.value_set update v;
      | Remove(k)   -> 
        let remove = Command.Operation.remove_init oper_rpc in
        Command.Operation.Remove.key_set_exn remove k);

      (Command.operation_set_builder cmd_rpc oper_rpc |> ignore);

      (* Constructs the command struct and associates with params *)
      (Params.command_set_reader params (Command.to_reader cmd_rpc) |> ignore);

      (* Send the message and pull out the result *)
      Capability.call_for_unit t method_id request >|= function
      | Ok () -> ()
      | Error e -> Hashtbl.clear sturdy_refs

let client_response_rpc t (cid : Types.command_id) (result : Types.result) =
  let open Api.Client.Message.ClientResponse in
  let request, params = Capability.Request.create Params.init_pointer in
  let open Api.Builder.Message in

  (* Create an empty result type as recognised by Capnp *)
  let result_rpc = Result.init_root () in
  
  (* As result is a Capnp union, match over the variant result argument
     and set the appropriate Capnp value of result_rpc *)
  (match result with
  | Failure ->
    Result.failure_set result_rpc
  | Success ->
    Result.success_set result_rpc
  | ReadSuccess v ->
    Result.read_set result_rpc v);

  (* Set the reader for the results union of the parameters *)
  Params.result_set_reader params (Result.to_reader result_rpc) |> ignore;

  (* Set the command id in the parameters to argument given *)
  Params.command_id_set_exn params cid;

  (* Send the message and ignore the response *)
      Capability.call_for_unit t method_id request >|= function
      | Ok () -> ()
      | Error e -> Hashtbl.clear sturdy_refs



let decision_rpc t (p : Types.proposal) =
  let open Api.Client.Message.Decision in
  let request, params = Capability.Request.create Params.init_pointer in
  let open Api.Builder.Message in

    (* Create an empty command type as recognised by Capnp *)
    let cmd_rpc = Command.init_root () in
    
    (* Construct a command struct for Capnp from the cmd argument given *)
    let (slot_number, ((id,uri), command_id, operation)) = p in
      Command.client_id_set cmd_rpc (Core.Uuid.to_string id);
      Command.client_uri_set cmd_rpc (Uri.to_string uri);
      Command.command_id_set_exn cmd_rpc command_id;
      (* Construct an operation struct here *)
      let oper_rpc = (Command.Operation.init_root ()) in
      
      (* Populate the operation struct with the correct values *)
      (match operation with
      | Nop         -> 
        Command.Operation.nop_set oper_rpc
      | Create(k,v) ->
        let create = (Command.Operation.create_init oper_rpc) in
        Command.Operation.Create.key_set_exn create k;
        Command.Operation.Create.value_set create v;
      | Read  (k)   -> 
        let read = Command.Operation.read_init oper_rpc in
        Command.Operation.Read.key_set_exn read k;
      | Update(k,v) ->
        let update = Command.Operation.update_init oper_rpc in
        Command.Operation.Update.key_set_exn update k;
        Command.Operation.Update.value_set update v;
      | Remove(k)   -> 
        let remove = Command.Operation.remove_init oper_rpc in
        Command.Operation.Remove.key_set_exn remove k);

      (Command.operation_set_builder cmd_rpc oper_rpc |> ignore);

      (* Constructs the command struct and associates with params *)
      (Params.command_set_reader params (Command.to_reader cmd_rpc) |> ignore);
      
      (* Add the given slot number argument to the message parameters *)
      Params.slot_number_set_exn params slot_number;

      (* Send the message and ignore the response *)
      Capability.call_for_unit t method_id request >|= function
      | Ok () -> ()
      | Error e -> Hashtbl.clear sturdy_refs


let proposal_rpc t (p : Types.proposal) =
  let open Api.Client.Message.SendProposal in
  let request, params = Capability.Request.create Params.init_pointer in
  let open Api.Builder.Message in
   (* Create an empty command type as recognised by Capnp *)
    let cmd_rpc = Command.init_root () in
    
    (* Construct a command struct for Capnp from the cmd argument given *)
    let (slot_number, ((id,uri), command_id, operation)) = p in
      Command.client_id_set cmd_rpc (Core.Uuid.to_string id);
      Command.client_uri_set cmd_rpc (Uri.to_string uri);
      Command.command_id_set_exn cmd_rpc command_id;
      (* Construct an operation struct here *)
      let oper_rpc = (Command.Operation.init_root ()) in
      
      (* Populate the operation struct with the correct values *)
      (match operation with
      | Nop         -> 
        Command.Operation.nop_set oper_rpc
      | Create(k,v) ->
        let create = (Command.Operation.create_init oper_rpc) in
        Command.Operation.Create.key_set_exn create k;
        Command.Operation.Create.value_set create v;
      | Read  (k)   -> 
        let read = Command.Operation.read_init oper_rpc in
        Command.Operation.Read.key_set_exn read k;
      | Update(k,v) ->
        let update = Command.Operation.update_init oper_rpc in
        Command.Operation.Update.key_set_exn update k;
        Command.Operation.Update.value_set update v;
      | Remove(k)   -> 
        let remove = Command.Operation.remove_init oper_rpc in
        Command.Operation.Remove.key_set_exn remove k);

      (Command.operation_set_builder cmd_rpc oper_rpc |> ignore);

      (* Constructs the command struct and associates with params *)
      (Params.command_set_reader params (Command.to_reader cmd_rpc) |> ignore);
      
      (* Add the given slot number argument to the message parameters *)
      Params.slot_number_set_exn params slot_number;

      (* Send the message and ignore the response *)
      Capability.call_for_unit t method_id request >|= function
      | Ok () -> ()
      | Error e -> Hashtbl.clear sturdy_refs



(*---------------------------------------------------------------------------*)

(* Types of message that can be passed between nodes:
      - This represents the application-level representation of a message.
      - These can be passed to the RPC api to be prepared for transport etc. *)
type message = ClientRequestMessage of command
             | ProposalMessage of proposal
             | DecisionMessage of proposal
             | ClientResponseMessage of command_id * result
          (* | ... further messages will be added *) 

(* Start a new server advertised at address (host,port)
   This server does not serve with TLS and the service ID for the
   server is derived from its address *)
let start_new_server ?request_callback ?proposal_callback ?response_callback ?phase1_callback ?phase2_callback host port =
    let listen_address = `TCP (host, port) in
    let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key:`Ephemeral listen_address in
    (* let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in *)
    let service_id = Capnp_rpc_lwt.Restorer.Id.derived ~secret:"" (host ^ (string_of_int port)) in
    let restore = Capnp_rpc_lwt.Restorer.single service_id (local ?request_callback ?proposal_callback ?response_callback ?phase1_callback ?phase2_callback () ) in
    Capnp_rpc_unix.serve config ~restore >|= fun vat ->
    Capnp_rpc_unix.Vat.sturdy_uri vat service_id;;

(* Resolve the URI for a given service from the host,port address pair *)
let uri_from_address host port =
  let service_id = Capnp_rpc_lwt.Restorer.Id.derived ~secret:"" (host ^ (string_of_int port)) in
  let service_id_str = Capnp_rpc_lwt.Restorer.Id.to_string service_id in
  let location = Capnp_rpc_unix.Network.Location.tcp host port in
  let digest = Capnp_rpc_lwt.Auth.Digest.insecure in
  Capnp_rpc_unix.Network.Address.to_uri ((location,digest),service_id_str)

(* Takes a Capnp URI for a service and returns the lwt capability of that
   service *)
let rec service_from_uri uri =
  (try Lwt.return (Some (Hashtbl.find sturdy_refs uri))
   with Not_found ->
    (try (  
    let client_vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
    Sturdy_ref.connect sr >>= function
    | Ok capability ->
      (Hashtbl.add sturdy_refs uri capability;
       Lwt.return_some capability)
    | Error e -> Lwt.return_none)
    with _ -> failwith "hello"))

(* Derive the service from an address by indirectly computing the URI.

   This is mostly for legacy reasons - all of the local node code sends
   messages based on URIs.

   TODO: Modify the code so that we don't need this extra indirection *)
let service_from_addr host port = 
  uri_from_address host port |> service_from_uri

(* Accepts as input a message and prepares it for RPC transport,
   given the URI of the service to which it will be sent*)
let send_request message uri =
  (* Get the service for the given URI *)
  service_from_uri uri >>= function
  | None -> Lwt.return_unit
  | Some service -> (
  match message with
  | ClientRequestMessage cmd ->
    client_request_rpc service cmd;
  | DecisionMessage p ->
    decision_rpc service p;
  | ProposalMessage p ->
    proposal_rpc service p;
  | ClientResponseMessage (cid, result) ->
    client_response_rpc service cid result)

(*---------------------------------------------------------------------------*)



















let phase1_rpc t (b : Ballot.t) =
  let open Api.Client.Message.Phase1 in
  let request, params = Capability.Request.create Params.init_pointer in
  let open Api.Builder.Message in
  Params.ballot_number_set params (b |> Ballot.serialize |> Yojson.Basic.to_string);  
  Capability.call_for_value_exn t method_id request >|=
  Results.result_get >|=
  Yojson.Basic.from_string >|=
  deserialize_phase1_response

(* TODO: Pattern matching here exhaustive *)
let send_phase1_message (b : Ballot.t) uri = 
  service_from_uri uri >>= function 
  | Some service -> 
  phase1_rpc service b;;

let phase2_rpc t (pval : Pval.t) =
  let open Api.Client.Message.Phase2 in
  let request, params = Capability.Request.create Params.init_pointer in
  let open Api.Builder.Message in
  Params.pvalue_set params (pval |> Pval.serialize |> Yojson.Basic.to_string);
  Capability.call_for_value_exn t method_id request >|=
  Results.result_get >|=
  Yojson.Basic.from_string >|=
  deserialize_phase2_response

(* TODO: Pattern matching here is not exhaustive *)
let send_phase2_message (pval : Pval.t) uri =
  service_from_uri uri >>= function
  | Some service ->
    phase2_rpc service pval
