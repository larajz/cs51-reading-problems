(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref ;;

(*......................................................................
Problem 1: Write a function `has_cycle` that returns whether a mutable
list has a cycle. You may want a recursive auxiliary function. Don't
worry about space usage.
......................................................................*)

(* let has_cycle (lst : 'a mlist) : bool =
  let rec iterated_has_cycle (visited_nodes : 'a mlist ref list)
                             (mlst : 'a mlist)
                             (curr_location : 'a mlist ref)
                            : bool =
  match mlst with
  | Nil -> false
  | Cons (_hd, tl) -> if (List.filter ((==) curr_location) visited_nodes) <> [] then true
                      else iterated_has_cycle (tl :: visited_nodes) !tl tl in
  (* This match case is required because the tail of the mlist contains
     the address of the next head. And we store the current location of the
     head by passing it through an argument. *)

  (* Also, an important point to be noted is that there is no way
     to find the address of the first element, thus we have to account
     foe the first element separately. This is why I thought that the
     next match case is necessary. --You're right, good note!*)

  (* There is a diagram on pg 307 of the book which helped me understand mlists!
     Might be worth checking out *)
  match lst with
  | Nil -> false
  | Cons (_hd, tl) -> iterated_has_cycle [] !tl tl ;; *)


(* alternative *)
(* let has_cycle2 (lst : 'a mlist) : bool =
  let rec cycle_helper (current : 'a mlist) (visited : 'a mlist ref list) : bool =
    match current with
    | Nil -> false
    | Cons (_, tl) ->
        match !tl with
        | Nil
        | Cons (_, {contents = Nil}) -> false
        | Cons (_, tl2) ->
          if List.exists (fun x -> x = tl2) visited then true
          else cycle_helper !tl (tl :: visited)
  in cycle_helper lst [] ;; *)


let rec ref_aux (current : 'a mlist)
    (visited : 'a mlist list)
    (counter : int)
  : int * 'a mlist ref option =
  match current with
  | Cons (hd, tl) ->
    (* if we have been here before, then we've reached the end of the nodes that could have cycles *)
    if (List.memq !tl visited) then (counter, Some tl)
(* otherwise *)
    else (ref_aux !tl (current :: visited) (counter + 1))
  | Nil -> (counter, None)

let has_cycle (lst : 'a mlist) : bool =
  not (snd (ref_aux lst [] 0) = None) ;;

(* let flatten (lst : 'a mlist) : unit =
  match snd (ref_aux lst [] 0) with
  | None -> ()
  | Some mlstref -> mlstref := Nil ;;

let mlength (lst : 'a mlist) : int =
   fst (ref_aux lst [] 0) ;; *)

   let flatten (lst : 'a mlist) : unit =
     let rec flat node prevs =
       match node with
       | Cons (_, r) -> if List.memq node prevs then r := Nil;
         flat (!r) (node::prevs)
       | Nil -> ()
     in flat lst [] ;;

     let mlength (lst : 'a mlist) : int =
       let rec count node prevs c =
         match node with
         | Cons (_, r) -> if List.memq node prevs then c else
           count (!r) (node::prevs) (c + 1)
         | Nil -> c
       in count lst [] 0 ;;
(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

(* let flatten (lst : 'a mlist) : unit =
  let rec iterated_flatten (visited_nodes : 'a mlist ref list)
                             (mlst : 'a mlist)
                             (curr_location : 'a mlist ref)
                            : unit =
  match mlst with
  | Nil -> ()
  | Cons (_hd, tl) -> if (List.filter ((==) curr_location) visited_nodes) <> []
                     then tl := Nil
                     else iterated_flatten (tl :: visited_nodes) !tl tl in
  match lst with
  | Nil -> ()
  | Cons (_hd, tl) -> iterated_flatten [] !tl tl ;; *)

(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
......................................................................*)

(* let mlength (lst : 'a mlist) : int =
  let rec iterated_length (visited_nodes : 'a mlist ref list)
                          (mlst : 'a mlist)
                          (curr_location : 'a mlist ref)
                          (len : int)
                         : int =
  match mlst with
  | Nil -> 0
  | Cons (_hd, tl) -> if (List.filter ((==) curr_location) visited_nodes) <> []
                     then len
                     else iterated_length (tl :: visited_nodes) !tl tl (len + 1) in
  match lst with
  (* If the first element is Nil then the length will be 0 *)
  | Nil -> 0
  (* If it filters through to this match case, it means that the mlist
     has atleast one element. This is why the argument for 'len' in
     iterated_length is set to 1 *)
  | Cons (_hd, tl) -> iterated_length [] !tl tl 1 ;; *)

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int =
  failwith "time estimate not provided" ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "...your reflections here..." ;;
