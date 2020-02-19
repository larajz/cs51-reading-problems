let prob1 (x,y) = x && y in prob1 ;;

let rec prob2 x =
  match x with
  | [] -> []
  | h :: t -> true :: prob2 t in prob2 ;;

let f x y = x && y in f;;

let f x =
  match x with
  | [] -> [true]
  | h :: t -> [true] ;;

(* problem 3 *)
let f x y z =
  if x (y,z) = y
  then
    y
  else
    y
in f ;;

(* problem 4 *)
let prob4 (x,y,z) l =
  if x = 0 && (List.hd l) = y then [z] else [] in prob4;;


(* problem 5 *)
let f x =
  if x then () else () ;;

(* problem 6 *)
let f x y =
  y (x y) in f;;



(* problem 1 *)
let f (x,y) = if x && y then y else y in f;;

(* problem 2 *)
let f alst = match alst with
  | [] -> [true]
  | hd :: tl -> [true] in f ;;

(* problem 3 *)
let problem3 f a b =
  if f a b = a then a else a ;;

(* problem 4 *)
let f (x,y,z) alst =
  match alst with
  | [] -> []
  | head :: tail -> if x = 0 then [z] else [z];;

(* need help on 4 *)
(* problem 6 *)
let rec f x f = f x ;;

(* wow good job! *)

let rec fold
    (f : int -> int -> int)
    (init : int)
    (xs : int list)
         : int =
    match xs with
      |[]->init
      |hd::tl->f hd(fold f init tl);;

      let rec fold
          (f : 'a -> 'a -> 'a)
          (init : 'a)
          (xs : 'a list)
               : 'a =
          match xs with
            |[]->init
            |hd::tl->f hd(fold f init tl);;

let rec filter (test : 'a -> 'b)
    (lst : 'a list)
  : 'a list =
  match lst with
  | [] -> []
  | hd :: tl -> if test hd
    then hd :: filter test tl
    else filter test tl ;;

(* problem 56 *)

(* 1. float -> float
   2. (int -> 'a) -> int -> 'a
   3. 'a list -> 'a list BUT NO IT DOESN'T TYPE!
   4. ('a -> 'a) list -> a' -> 'a
   5. (bool * 'a) -> ('a -> bool) -> bool *)

(* number 6 *)
let rec f x j = j x;;

(* problem 57 *)
let map f lst = List.fold_right (fun other lst2 -> f other :: lst2) lst [] ;;

(* problem 59 *)
let (@+) f g x = f (g x) ;;

(* problem 61 *)
(* hd : 'a list -> a , tl : 'a list -> a list *)


(* problem 4, exercise 54*)
let f (i,a,b) alst =
  if i = 0 then [b]
  else if (List.hd(alst) = a) then [b]
  else [b]
;;

(* problem 5 *)
let f (x : bool) =
  if x then ();;

(* problem 6 *)
let g (thing : 'a) (f : 'a -> 'b) : 'b =  f thing;;


(* exercise 55 *)
let rec fold
    (f : 'a -> 'a -> 'a)
    (init : 'a)
    (xs : 'a list)
         : 'a =
    match xs with
      | [] -> init
      | hd :: tl -> f hd(fold f init tl);;

(* exercise 56 *)
(*
1. x is a float, so f : float -> float
2. (int -> 'a) -> int -> 'a
3. x is a list, then it will return the list, but it also returns an element of the list? it won't type
4. ('a -> 'a) list -> 'a -> 'a
*)
(* exercise 57 *)
let mapper (f) (lst : 'a list) : 'a list=
  List.fold_right (fun x y -> (f x) :: y) lst [];;
(* exercise 58 *)
(* map returns a list, and fold_right doesn't necessarily *)
