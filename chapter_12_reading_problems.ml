(* exercise 94*)
  module IntQueue =
  struct
  type int_queue = int list
  let empty_queue : int_queue = []
  let enqueue (elt : int) (q : int_queue) : int_queue = [elt] @ q
  let dequeue (q : int_queue) : int * int_queue =
  match q with
  | [] -> raise (Invalid_argument
  "dequeue: empty queue")
  | hd :: tl -> hd, tl
end ;;

(* problem 99 *)

module MakeImaging (thing : PIXEL) : (IMAGING with pixel = thing.t) =
struct
  (* stuff
  *)
end ;;
