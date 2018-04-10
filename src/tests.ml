open OUnit
open Core.List
open Types
open Ballot

(*

(* Tests for ballot equality *)
let ballot_equality_test test_ctx =
  let b = Ballot.Bottom and b' = Ballot.Bottom in
  assert_bool "Foo" (equal b b')

(* Test if a number and a bottom are equal *)
let ballot_equality_test2 test_ctx =
  assert_bool "Foo" (equal (Ballot.Bottom) (Ballot.Number(0,create_id ())))


(*
   Bottom, bottom
   Bottom, number and id

   Same number, same id
   Different number, same id
   same id, different number
   Different number, different id

   Negative ids
*)

let ballot_equality_tests = [
  "..." >:: 
  (fun ctx ->
    equal Bottom Bottom |> assert_bool "Foo");
  
  "..." >:: 
  (fun ctx ->
    equal Bottom (Number(0,create_id())) |> not |> assert_bool "Foo");

  "..." >:: 
  (fun ctx -> let id = create_id () in    
    equal (Number(0,id)) (Number(0,id)) |> assert_bool "Foo");

  "..." >:: 
  (fun ctx ->
    equal (Number(0,create_id())) (Number(0,create_id())) |> not |> assert_bool "Foo");

  "Different number, different id" >:: 
  (fun ctx ->
    equal (Number(0,create_id())) (Number(1,create_id())) |> not |> assert_bool "Foo");
]

(*
   Check that bottom is less than everything

   Check the lexicographic ordering, i.e.
      - Given that for whatever the ids are, the numbers order them ballots
      - Only when the numbers are equal does id matter
*)

(* Helper function generates a pair of unique ids such that
   the first is less than the second *)
let two_ordered_ids () =
  let id1, id2 = create_id (), create_id () in
  if Core.Uuid.compare id1 id2 < 0 
  then id1, id2
  else id2, id1

let ballot_ordering_tests = [
  "Bottom is least ballot" >::
  (fun ctx ->
    let tst b = less_than Bottom b in
    let bs = init 10 ~f:(fun x -> Number(x,create_id())) in 
    for_all bs ~f:tst |> assert_bool "Foo");

  "Check lexicographical ordering of ballots with the same id" >::
  (fun ctx ->
    let id = create_id () in
    (less_than (Number(0,id)) (Number(1,id)) &&
    (less_than (Number(1,id)) (Number(0,id)) |> not)) |> assert_bool "Foo");

  "Check lexicographical ordering of ballots with different ids" >::
  (fun ctx ->
    let id1, id2 = two_ordered_ids () in
    (less_than (Number(0,id1)) (Number(1,id2)) &&
     less_than (Number(1,id1)) (Number(0,id2)) |> not &&
     less_than (Number(0,id2)) (Number(1,id1)) && 
     less_than (Number(1,id2)) (Number(0,id1)) |> not) |> assert_bool "Foo");

  "Check that given same integer, ballots are ordered by their ids" >::
  (fun ctx ->
     let id1, id2 = two_ordered_ids () in
     (less_than (Number(0,id1)) (Number(0,id2)) &&
      less_than (Number(0,id2)) (Number(0,id1)) |> not) |> assert_bool "Foo");

  "Check that comparisons with bottom yield expected integer result" >::
  (fun ctx ->
     let id = create_id () in
     Ballot.compare Bottom (Number(0,id)) |> assert_equal (-1));

  "Check that comparisons with bottom yield expected integer result" >::
  (fun ctx ->
     Ballot.compare Bottom Bottom |> assert_equal 0);

  "Check that comparisons with bottom yield expected integer result" >::
  (fun ctx ->
     let id = create_id () in
     Ballot.compare (Number(0,id)) Bottom |> assert_equal 1);

  "Check comparison is made first based on integer part of ballot" >::
  (fun ctx ->
     let id1, id2 = create_id (), create_id () in
     (((Ballot.compare (Number(0,id1)) (Number(1,id2))) < 0 ) &&
     ((Ballot.compare (Number(0,id2)) (Number(1,id1))) < 0 )) |> assert_bool "Foo");

  "Check comparison is made first based on integer part of ballot" >::
  (fun ctx ->
     let id1, id2 = create_id (), create_id () in
     (((Ballot.compare (Number(1,id1)) (Number(0,id2))) > 0 ) &&
      ((Ballot.compare (Number(1,id2)) (Number(0,id1))) > 0 )) |> assert_bool "Foo"); 

  "Check comparison is made first based on integer part of ballot" >::
  (fun ctx ->
     let id = create_id () in
     ((Ballot.compare (Number(0,id)) (Number(0,id))) = 0 ) |> assert_bool "Foo");
]






(* 
   x << y returns all the elements in y and all the elements of x not in y


   - Strategy: 

   [] << [] == []
   xs << [] == xs
   xs << ys == xs, for-all ys not in xs
*)


(* Tests for operations on proposals *)
open Leader
open Types


(*
   Tests:
   - empty list throws exception
   - given a list of numbers with same leader id, select highest round number
   - given a list of numbers with same round no, select highest leader id
   - given a mix, select highest
   - given two of the same, select just one
*)

(* Test maximum ballot function *)

(* Want a helper function that takes list of pvalues and a pvalue and confirms
   whether function is correct *)

(* pvalue = ballot.t * slot_number * command *)
let f =
  "max_ballot tests" >:::
  (List.map
    (fun (pvals,max) ->
    let title = 
      "Test pvals"
    in
      title >::
      (fun ctx ->
         assert_equal (max_ballot pvals) max))
    [
    ])


let test1 =
  let id = create_id () in
  let cid = create_id (), Uri.of_string "" in
  [ 
    [(Number(0,id),0,(cid,0,Nop))], (Number(0,id),0,(cid,0,Nop))
  ]

(* Generate list of ordered leader ids *)
let gen_ordered_lids n = 
  let rec gen n = match n with
    | 0 -> []
    | n -> create_id () :: gen (n-1)
  in
    stable_sort ~cmp:Core.Uuid.compare (gen n)

(* Generate a simple pval from a ballot.
   Since we are testing on the ordering of ballots these values do not matter here *)
let pval_from_ballot (b : Ballot.t) : Ballot.pvalue = 
  (b, 0, ((create_id (), Uri.of_string ""), 0, Nop))

(* Generate list of n pvalues with same ballot round number 0 *)
let gen_pvals n = map (gen_ordered_lids n) ~f:(fun l -> pval_from_ballot ( Number(0,l) ))


*)


(* Equality tests *)



(* Test that calling successor on the bottom ballot raises a successor exception *)
let ballot_successor_exn_test =
  let open Ballot in
  "Check bottom has no successor" >:: 
  (fun ctx ->
     assert_raises SuccessorException 
       (fun () -> succ_exn ( bottom () )))

(* Run a set of tests that test that ballot equality operates as expected *)
let ballot_equality_tests = 
  let open Ballot in
  "Ballot equality tests" >:::
  map ~f:(fun (arg1, arg2, res) -> 
      let title = 
        "Test " ^ (to_string arg1) ^ " == " ^ (to_string arg2) ^ " is " ^ (string_of_bool res)
      in
        title >::
        (fun ctx ->
           assert_equal (equal arg1 arg2) res))
    [
      (* bottom and bottom are equal *)
      bottom (), bottom (), true;

      (* bottom and some initial ballot are not equal *)
      init (create_id ()), bottom (), false;

      (* ...same again but args switched *)
      bottom (), init (create_id ()), false;

      (* Check initial ballots with same id are equal *)
      (let id = create_id () in init id, init id, true);

      (* Check initial ballots with different ids are not equal *)
      init (create_id ()), init (create_id ()), false;

      (* Check a ballot and its successor are not equal *)
      (let b = init (create_id ()) in b, succ_exn b, false);

      (* Check a ballot and some other ballots successor are not equal *)
      init (create_id ()), succ_exn (init (create_id ())), false;
    ]

(* Run a set of tests that test ordering of ballots *)
let ballot_ordering_tests = 
  let open Ballot in
  
  (* Helper function generates n-element list of n successors of b
    (not including b) *)
  let rec succ_n_list n b =
    if n <0 then failwith "Negative n not allowed" else
    match n with
    | 0 -> []
    | n' -> (succ_exn b) :: ( succ_n_list (n-1) (succ_exn b) ) in

  (* Generate a pair (id1, id2) of ids such that id1 < id2 *)
  let gen_ord_ids () =
    let open Core.Uuid in
    let id, id' = create_id (), create_id () in
      if id < id' then (id, id')
                  else (id', id) in

  (* Generate a list of (b1,b2,res) triples, where each is a ballot and some
     successor of that ballot. *)
  let gen_succs n b =
    let bs = b :: (succ_n_list n b) in
      let bss = mapi bs ~f:(fun i -> fun b' ->
        map ~f:(fun b'' -> b', b'', true) ( (succ_n_list (n-i) b'))) in
          (b, succ_exn b, true) :: (concat bss) in

  (* Suite of tests *)
  "Ballot ordering tests" >:::
  map ~f:(fun (arg1, arg2, res) -> 
      let title = 
        "Foo"
      in
        title >::
        (fun ctx ->
           assert_equal (less_than arg1 arg2) res))
    ([
      (* Check bottom is not less than bottom *)
      bottom (), bottom(), false;

      (* Check bottom is less than some initial ballot *)
      bottom (), init (create_id ()), true;
      init (create_id ()), bottom (), false;
      
      (* Check that successors of ballots are less than previous ballots *)
      (let b = init (create_id ()) in b, succ_exn b, true);
      (let b = init (create_id ()) in succ_exn b, b, false);

      (* Generate a pair of ordered initial ballots, compare them as initial ballots *)
      (let id1, id2 = gen_ord_ids () in init id1, init id2, true);
      (let id1, id2 = gen_ord_ids () in init id2, init id1, false);

      (* Generate two ordered ids and check the lexicographic ordering of ballots.
         We only check against single successor here, given that if earlier tests in
         this set have passed it'll hold for greater ballots *)
      (let id1, id2 = gen_ord_ids () in init id2, succ_exn (init id1), true);
      (let id1, id2 = gen_ord_ids () in succ_exn (init id1), init id2, false);
      (let id1, id2 = gen_ord_ids () in succ_exn (init id1), succ_exn (init id2), true);
      (let id1, id2 = gen_ord_ids () in succ_exn (init id2), succ_exn (init id1), false);
    ]
      @ (* Check first ten successors of an id are greater than bottom *)
      ( map ~f:(fun b -> bottom (), b, true ) 
          (succ_n_list 10 (init (create_id ()))) )
      (* Check that the ten successors of b are less than each successor before it *)
      @ (gen_succs 10 (init (create_id ()))))


(* Test comparison for ballots ... *)
(* ... *)


(* Suite of serialize and deserialize tests for ballots ... *)
(* ... *)



let ballot_test_suite = "Ballot test suite" >:::
                        [ ballot_equality_tests; 
                          ballot_successor_exn_test;
                          ballot_ordering_tests; ]



(* UNCOMMENT ONCE WE SORT THE APPLICATION OUT ----------

(* Application state tests *)
let app_state_tests =
  let open Types.Store in
  let s = [(1,"x");(3,"z");(2,"y");(4,"w")] in
  "Application tests" >::: 
  map ~f:(fun (state,op,res) ->
    let title = 
      "Apply operation " ^ (op_to_string op) ^
      " to " ^ (to_string state) ^
      ", should get " ^ (to_string (fst res)) ^ "," ^ (result_to_string (snd res))
    in
      title >:: 
      (fun ctx ->
         assert_equal (apply state op) res))
    [ [], Nop, ([],Success);
      [], Create(1,"x"), ([(1,"x")],Success);
      [], Read(1), ([],Failure);
      [], Update(1,"k"), ([], Failure);
      [], Remove(1), ([], Failure);
      s, Nop, (s,Success);
      s, Create(5,"v"), ((5,"v")::s,Success);
      s, Create(1,"y"), (s, Failure);
      s, Read(1), (s,ReadSuccess("x"));
      s, Read(3), (s,ReadSuccess("z"));  
      s, Read(4), (s,ReadSuccess("w"));  
      s, Read(100), (s, Failure);
      s, Update(1,"y"), ([(1,"y");(3,"z");(2,"y");(4,"w")],Success);
      s, Update(3,"y"), ([(3,"y");(1,"x");(2,"y");(4,"w")],Success);  
      s, Update(4,"y"), ([(4,"y");(1,"x");(3,"z");(2,"y")],Success);    
      s, Update(5,"y"), (s, Failure);
      s, Remove(1), ([(3,"z");(2,"y");(4,"w")],Success);
      s, Remove(3), ([(1,"x");(2,"y");(4,"w")],Success);
      s, Remove(4), ([(1,"x");(3,"z");(2,"y")],Success);
      s, Remove(100), (s, Failure) ]

(* Test equality functions for commands and proposals *)
let equality_tests = 
  let open Types.Store in
  (* Override uri creation function here to save space *)
  let create_id () = create_id (), Uri.of_string "" in
  "Equality tests" >:::
  append 
    (map ~f:(fun (c1,c2,res) ->
      let title =
        "Test equality of " ^ (string_of_command c1) ^ " and " ^ (string_of_command c2)
      in
        title >::
        (fun ctx ->
           assert_equal (commands_equal c1 c2) res))
    [ (create_id (), 0, Nop), (create_id (), 0, Nop), false;
      (create_id (), 0, Nop), (create_id (), 1, Nop), false;    
      (create_id (), 0, Nop), (create_id (), 0, Read(10)), false; 
      (create_id (), 1, Create(10,"x")), (create_id (), 0, Read(10)), false; 
      (let id = create_id () in
        (id, 0, Nop), (id, 1, Nop), false);
      (let id = create_id () in
        (id, 0, Nop), (id, 0, Update(4,"x")), false);
      (let id = create_id () in
        (id, 1, Read(10)), (id, 0, Update(4,"x")), false);
      (let id = create_id () in
        (id, 0, Nop), (id, 0, Nop), true);
      (let id = create_id () in
        (id, 1, Read(10)), (id, 1, Read(10)), true);
      (let id = create_id () in
       (id, 1, Update(1,"x")), (id, 1, Update(1,"x")), true) ])
    (map ~f:(fun (p1,p2,res) ->
      let title =
        "Test equality of " ^ (string_of_proposal p1) ^ " and " ^ (string_of_proposal p2)
      in
        title >::
        (fun ctx ->
          assert_equal (proposals_equal p1 p2) res))
    [ (0,(create_id (), 0, Nop)), (0,(create_id (), 0, Nop)), false;
      (0,(create_id (), 0, Nop)), (0,(create_id (), 1, Nop)), false;    
      (0,(create_id (), 0, Nop)), (0,(create_id (), 0, Read(10))), false; 
      (0,(create_id (), 1, Create(10,"x"))), (0,(create_id (), 0, Read(10))), false;
      (0,(create_id (), 0, Nop)), (1,(create_id (), 0, Nop)), false;
      (0,(create_id (), 0, Nop)), (1,(create_id (), 1, Nop)), false;    
      (0,(create_id (), 0, Nop)), (1,(create_id (), 0, Read(10))), false; 
      (0,(create_id (), 1, Create(10,"x"))), (1,(create_id (), 0, Read(10))), false;
      (let id = create_id () in
        (0,(id, 0, Nop)), (0,(id, 1, Nop)), false);
      (let id = create_id () in
        (0,(id, 0, Nop)), (0,(id, 0, Update(4,"x"))), false);
      (let id = create_id () in
        (0,(id, 1, Read(10))), (0,(id, 0, Update(4,"x"))), false);
      (let id = create_id () in
        (0,(id, 0, Nop)), (1,(id, 1, Nop)), false);
      (let id = create_id () in
        (0,(id, 0, Nop)), (1,(id, 0, Update(4,"x"))), false);
      (let id = create_id () in
        (0,(id, 1, Read(10))), (1,(id, 0, Update(4,"x"))), false);
      (let id = create_id () in
        (0,(id, 0, Nop)), (1,(id, 0, Nop)), true);
      (let id = create_id () in
        (0,(id, 1, Read(10))), (1,(id, 1, Read(10))), true);
      (let id = create_id () in
        (0,(id, 1, Update(1,"x"))), (1,(id, 1, Update(1,"x"))), true);
      (let id = create_id () in
        (0,(id, 0, Nop)), (0,(id, 0, Nop)), true);
      (let id = create_id () in
        (0,(id, 1, Read(10))), (0,(id, 1, Read(10))), true);
      (let id = create_id () in
        (0,(id, 1, Update(1,"x"))), (0,(id, 1, Update(1,"x"))), true) ] )
*)















(* Collect together tests and run them *)
let run_tests = run_test_tt ballot_test_suite
