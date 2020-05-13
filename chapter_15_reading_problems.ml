(* 163 *)
(*
1. a points to b, b points to 5.
so !a is b, !b is 5.  int = 5

2. doesn't type

3. b points to a which points to 1
new a doesn't affect the old a
!b is a, !a is 1

4. function multiplies a number by 2
f 3 = 6
applying it again gives 12

5. where's the 'a mlist type definition?

6. ^
*)

(* To update a mutable record,
   the operator <- is used,
   rather than := as for references. *)

(* 165 *)
let p = ref 11 ;;
(* p is an int ref *)
let r = ref p ;;
(* r is an int ref ref *)
let s = ref !r ;;
(* now s points to p also
   3a. false
   3b. true
   3c. false
   3d. true
*)
let t =
  !s := 14;
  !p + !(!r) + !(!s)
;;
(* since s points to p, !s := 14 goes to p and changes *)


type 'a mlist =
  | Nil
  | Cons of 'a * ('a mlist ref)

(* 166 *)
let mhead (Cons (hd, _tl)) = hd ;;
let mtail (Cons (_hd, tl)) = !tl ;;

(* 167 *)
let rec length_helper
    (lst : 'a mlist)
    (acc : 'a mlist ref list)
    (counter : int)
  : int
  =
  match lst with
  | Nil -> counter
  | Cons (hd, tlref) ->
    if List.memq tlref acc then (counter + 1)
    else length_helper !tlref
        (tlref :: acc) (counter + 1) ;;

let length (lst : 'a mlist) : int =
  length_helper lst [] 0 ;;
(* testing *)
let r = ref Nil ;;
let s = Cons(1, r) ;;
let t = Cons(2, ref s);;
r := t ;;

(* checks out *)


(* 168 *)

let rec first_helper (mlist : 'a mlist) (n : int) (acc: 'a list) : 'a list =
  if n = 0 then acc
  else
    match mlist with
    | Nil -> acc
    | Cons(hd, tlref) -> first_helper (!tlref) (pred n) (acc @ [hd]) ;;

let rec first (n : int) (mlst : 'a mlist) : 'a list =
   if n < 0 then raise (Invalid_argument "first: negative integer")
   else if n = 0 then []
   else first_helper mlst n [];;

(* 169 *)
(* let alternating_contents = ref Nil ;;
let s = Cons(1, r) ;;
let t = Cons(2, ref s);;
alternating_contents := t ;;
let alternating = !alternating_contents ;; *)
(* first 10 alternating ;; *)

let initial = ref Nil
let ones = Cons(2, initial) ;;
let twos = Cons(1, ref ones) ;;
initial := twos ;;
let alternating = twos ;;
first 10 alternating ;;

(* chapter 16 *)
(*
procedural programming emphasizes the steps to be carried out
space efficiency
while <expression condition> do <expression body> done
Tail-recursive functions need not use a
stack to keep track of suspended computations :)
 *)

(* chapter 17 *)
(*
The body of a function is not evaluated until
the function is applied.
 *)
(* 178 Rewrite tail,smap,smap2,and first
   to use the Lazy module. *)
type 'a stream_internal = Cons of 'a * 'a stream
and 'a stream = 'a stream_internal Lazy.t ;;

let head (s : 'a stream) : 'a = match Lazy.force s with
  | Cons (hd, _tl) -> hd ;;

let tail (s : 'a stream) : 'a stream =
  match Lazy.force s with
  | Cons (_hd, tl) -> tl ;;

let rec smap (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  match Lazy.force s with
  | Cons (hd, tl) -> lazy (Cons (f hd, smap f tl)) ;;

let rec smap2 f s1 s2 =
  lazy (Cons(f (head s1) (head s2),
             smap2 f (tail s1) (tail s2))) ;;

let rec first (n : int) (s : 'a stream) : 'a list =
  if n = 0 then []
  else (head s) :: first (pred n) (tail s) ;;

let rec fibs =
  lazy (Cons (0,
              lazy (Cons (1,
                          smap2 (+) fibs (tail fibs))))) ;;

let rec sums s =
  smap2 ( +. ) s (lazy (Cons (0.0, sums s))) ;;

let rec fibratios (s : 'a stream) : 'a stream =
  lazy (Cons ((head (tail s)) /.  (head s), fibratios (tail s)));;
