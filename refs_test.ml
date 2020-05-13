(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                             Refs Testing
 *)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons (2, ref Nil) ;;
let list1b = Cons (2, ref list1a) ;;
let list1 = Cons (1, ref list1b) ;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;







let list0 = Cons(2,ref Nil);;

let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b);;

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2;;

let reflist2 = ref (Cons(2, ref Nil))
let list3 = Cons(1, ref (Cons (2, ref(Cons (3, reflist2)))))
let _ = reflist2 := list3;;

(* Some example tests. You'll want more. *)
let _ =
  assert(not(has_cycle list1a)) ;
  assert(has_cycle(!reflist)) ;
  assert(not(has_cycle list1b));
  assert (not(has_cycle list1)) ;


  assert (mlength list0 = 1);
assert (mlength list1 = 3);
assert (mlength list2 = 2);
assert (mlength list3 = 3);

assert (flatten list0; list0 = Cons (2, {contents = Nil}));
assert (flatten list1; list1 = Cons (1, {contents = Cons (2, {contents = Cons (2, {contents = Nil})})}));
assert (flatten list2; list2 = Cons (1, {contents = Cons (2, {contents = Nil})}));
assert (flatten list3; list3 = Cons (1, {contents = Cons (2, {contents = Cons (3, {contents = Nil})})}));






;;
