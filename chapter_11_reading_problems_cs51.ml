(* exercise 81 *)
(* |> is 'a -> ('a -> 'b) -> 'b *)

(* 82 *)
let f (a : 'a) (func : 'a -> 'b) : 'b =
  func a ;;

(* 83 Define a version of eval that implements a different
semantics for the expression language, for instance, by rounding rather
than truncating integer divisions *)
type expr =
Int of int
| Plus of expr * expr
| Minus of expr * expr
| Times of expr * expr
  | Div of expr * expr
  | Neg of expr ;;
open Float ;;
open Int ;;

let rec eval (exp : expr) : int =
   match exp with
   | Int v -> v
   | Plus (x, y) -> eval x + eval y
   | Minus (x, y) -> eval x - eval y
   | Times (x, y) -> eval x * eval y
   | Div (x, y) -> to_int (to_float (eval x) /. to_float (eval y))
   | Neg x -> ~- (eval x) ;;
(* problem 85 *)
type suit =
  | Spade | Diamond ;;


type cardval =
  | King | Queen | Jack ;;


(* problem 88 *)
let 'a tree =
| Empty
| Leaf of 'a
| Node of (tree, tree) ;;
let sum_tree (t : int tree) =
match t with
| Leaf x -> x
| Node (l, r) -> (sum_tree l) + (sum_tree r) ;;
