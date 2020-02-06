let computus_month (y : int) : int =
let a = y mod 19 in
let b = y / 100 in
let c = y mod 100 in
let d = b/4 in
let e = b mod 4 in
let f = (b + 8) / 25 in
let g = (b-f+1)/3 in
let h = (19 * a + b - d - g + 15) mod 30 in
let i = c / 4 in
let k = c mod 4 in
let l = (32 + 2 * e + 2 * i - h - k) mod 7 in
let m =( a + 11 * h + 22 * l)/451 in
(h+l-7 * m + 114) / 31;;


let computus_day (y : int) : int =
let a = y mod 19 in
let b = y / 100 in
let c = y mod 100 in
let d = b/4 in
let e = b mod 4 in
let f = (b + 8) / 25 in
let g = (b-f+1)/3 in
let h = (19 * a + b - d - g + 15) mod 30 in
let i = c / 4 in
let k = c mod 4 in
let l = (32 + 2 * e + 2 * i - h - k) mod 7 in
let m =( a + 11 * h + 22 * l)/451 in
(h+l-7 * m + 114) mod 31 + 1 ;;

(* exercise 33 february 5, 2020 *)
let computus_both (y : int) : int * int =
let a = y mod 19 in
let b = y / 100 in
let c = y mod 100 in
let d = b/4 in
let e = b mod 4 in
let f = (b + 8) / 25 in
let g = (b-f+1)/3 in
let h = (19 * a + b - d - g + 15) mod 30 in
let i = c / 4 in
let k = c mod 4 in
let l = (32 + 2 * e + 2 * i - h - k) mod 7 in
let m =( a + 11 * h + 22 * l)/451 in
(h+l-7 * m + 114) / 31 , (h+l-7 * m + 114) mod 31 + 1 ;;

(* exercise 34 *)
let snd (pair : int * int) : int =
  match pair with
  | _x, y -> y ;;

(* watch out for style in the style guide! *)

(* exercise 35 *)
let addpair (x , y : int * int) : int = x + y ;;

let fst (x, _y : int * int) : int = x ;;

(* exercise 36 *)
let slope (x1 , y1 : float * float)
          (x2 , y2 : float * float)
            : float =
  (y2 -. y1) /. (x2 -. x1) ;;

(* exercise 37 *)
(* 1 is well formed, 2 YES because [false] is
   syntactic sugar, 3 NO, 4 No, 5 NO *)

(* the length function expects an int list argument.
   but we gave it an int list list, a list of int lists*)

let rec prods (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x,y) :: tail -> x * y :: prods tail ;;

(* exercise 39 *)
let rec sum (lst : int list) : int =
  match lst with
  | [] -> 0
  | head :: tail -> head + sum tail ;;

(* exercise 40 *)
let rec prod (lst : int list) : int =
  match lst with
  | [] -> 1
  | head :: tail -> head * prod tail ;;

(* exercise 41 *)
let rec sums (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x,y) :: tail -> x + y :: sums tail ;;

(* exercise 42 *)
let rec inc_all (lst : int list) : int list =
  match lst with
  | [] -> []
  | head :: tail -> (succ head) :: inc_all tail ;;

(* exercise 43 *)
let rec square_all (lst : int list) : int list =
  match lst with
  | [] -> []
  | head :: tail -> (head * head) :: square_all tail ;;

(* exercise 44 *)
let rec append (lst1 : int list) (lst2 : int list) : int list =
  match lst1 with
  | [] -> lst2
  | head1 :: tail1 -> head1 :: append tail1 lst2 ;;


(* the fold abstraction *)
let rec fold (f : int -> int -> int)
    (init : int)
    (xs : int list)
  : int =
  match xs with
  | [] -> init
  | head :: tail -> f head (fold f init tail) ;;
(* the repeated embedded application of a binary function,
   starting with an initial value, to all of the elements
   of a list*)

(* exercise 45 *)
(* left to right fold *)



let rec fold_left (f : int -> int -> int)
    (acc : int)
    (xs : int list)
  : int =
  match xs with
  | [] -> acc
  | head :: tail -> fold_left f (f acc head) tail ;;

(* exercise 46 *)
(* first, the nonrecursive version using fold_left *)
let reduce (f : int -> int -> int)
    (lst : int list)
  : int =
  match lst with
  | [] -> 1
  | head :: tail -> fold_left f head tail ;;

(* now, for the recursive version *)
let rec reduce (f : int -> int -> int)
    (lst : int list)
  : int =
  match lst with
  | [] -> 0
  | head :: tail -> f head (reduce f tail) ;;

(* exercise 47 *)
let sum (lst : int list) : int = reduce (+) lst ;;
let prod (lst : int list) : int = reduce ( * ) lst ;;

(* exercise 48 *)
let rec map (f : int -> int) (xs : int list) : int list =
  match xs with
  | [] -> []
  | head :: tail -> f head :: (map f tail) ;;

(* the map function takes two arguments, the first of which is
   itself a function, to be applied to all elements of its
   second integer list argument *)

let rec fold (f : int -> int -> int)
       (init : int)
       (xs : int list)
     : int =
     match xs with
     | [] -> init
     | head :: tail -> f head (fold f init tail) ;;
(* HELP! *)

(* exercise 50 *)
let rec filter (f : int -> bool) (lst : int list) : int list =
  match lst with
  | [] -> []
  | head :: tail -> if f head then head :: filter f tail
    else filter f tail ;;

filter (fun x -> x mod 2 <> 0) [3;4;5] ;;

(* exercise 51 *)
let evens = filter (fun x -> x mod 2 = 0) ;;
let odds = filter (fun x -> x mod 2 <> 0) ;;
let positives = filter (fun x -> x > 0) ;;
let negatives = filter (fun x -> x < 0) ;;

(* exercise 52 *)
let reverse (lst : int list) : int list =
  List.fold_right (fun head tailvalue -> tailvalue @ [head]) lst [] ;;

(* alternatively, *)
let reverse (lst : int list) : int list =
  List.fold_left (fun prefixvalue next -> next :: prefixvalue) [] lst ;;


(* exercise 53 *)
let rec append (lst1 : int list) (lst2 : int list) : int list =
