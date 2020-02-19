(* exercise 81 *)
(* |> is 'a -> ('a -> 'b) -> 'b *)

(* 82 *)
let f (a : 'a) (func : 'a -> 'b) : 'b =
  func a ;;
