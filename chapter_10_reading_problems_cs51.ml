(* 62. so that you can partially apply it *)

(* 63. right now, nth_opt goes forever
   if you have a negative index *)

let rec nth_opt (lst : 'a list) (n : int) : 'a option =
  match lst with
  | [] -> None
  | hd :: tl ->
    if
      n < 0
    then
      None
    else if
      n = 0
    then
      Some hd
    else
      nth_opt tl (n-1) ;;

(* better solution for 63 *)

let rec nth_opt (lst : 'a list) (n : int) : ('a option) =
  if n < 0 then None else
    match lst with
    | [] -> None
    | hd :: tl -> if n = 0 then Some hd else nth_opt tl (n - 1) ;;


let rec last_opt (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | [something] -> Some something
  | _ :: tail -> last_opt tail ;;

let variance (lst : float list) : float option =
  let rec sum_length (lst : float list) : (float * int) =
    match lst with
    | [] -> (0.0,0) ;
    | hd :: tl ->
      let (sum, length) = sum_length tl in
      (hd +. sum, length + 1) in
  let (sum, length) = sum_length lst in
  if length < 2 then None
  else let (mean : float) = sum /. (float length) in
    let rec residuals (lst : float list) : float =
      match lst with
      | [] -> 0.
      | hd :: tl -> (hd -. mean) ** 2. +. residuals tl
    in Some (residuals lst /. (float length -. 1.)) ;;

(* exercise 67 *)
(* _,_ matches everything and then we never get to the useful branches *)
 let rec zip (xs : 'a list)
 (ys : 'b list)
 : ('a * 'b) list =
 match xs, ys with
 | [], [] -> []
 | [], _
 | _, [] -> raise (Invalid_argument
 "zip: unequal length lists")
 | xhd :: xtl, yhd :: ytl ->
 (xhd, yhd) :: (zip xtl ytl) ;;

(* exercise 68 *)
let zip_safe (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list =
  try (zip lst1 lst2) with Invalid_argument _ -> [];;

(* exercise 69 *)

let rec fact (n : int) : int =
  if
    n < 0
  then
    raise (Invalid_argument "factorial negative")
  else if
    n = 0
  then
    1
  else
    n * fact (n-1);;

(* exercise 70 *)
(*
1. int option
2. int option list
3. 'a option list
4. exn  (= Stdlib.Exit)
5. exn
6. Exception:
7. exn -> 'a
8. 'a -> 'b (because it could be anything)
9. string -> 'a
10. 'a -> 'b
11. int -> bool -> int list option
*)

(* exercise 71 *)
let f x y : int option =
  Some (x + y) ;;

let f g : int option =
  Some (g 1) ;;

let f x g =
  g x ;;

let f alst blst =
  match alst, blst with
  | None, None -> []
  | Some x, Some y -> [(x,y)]
  | _, _ -> [] ;;

(* problem 72 *)
(* 2. bool -> (bool * bool) *)

(* problem 73 *)
let f (x : bool) =  (true, true) ;;
