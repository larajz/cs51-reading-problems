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

let rec last_opt (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | [something] -> Some something
  | _ :: tail -> last_opt tail ;;

let variance (lst : float list) : float option =
  match lst with
  |
