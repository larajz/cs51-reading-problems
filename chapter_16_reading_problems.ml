 module type METERED =
 sig
 (* reset () -- Resets the count of allocations *)
 val reset : unit -> unit
 (* count () -- Returns the number of allocations
 since the last reset *)
 val count : unit -> int
 (* cons hd tl -- Returns the list cons of `hd` and
 `tl`, updating the allocation count accordingly *)
 val cons : 'a -> 'a list -> 'a list
 (* pair first second -- Returns the pair of `first`
 and `second`, updating the allocation count
 accordingly *)
 val pair : 'a -> 'b -> 'a * 'b
 end

module Metered : METERED = struct
  let counter = ref 0
  let reset () : unit = counter := 0
  let count () : int = !counter
  let cons hd tl = counter := !counter + 1; hd :: tl
  let pair first second = counter := !counter + 1; (first, second)
end



   let rec zip (xs : 'a list)
   (ys : 'b list)
   : ('a * 'b) list =
   match xs, ys with
   | [], [] -> []
   | [], _
   | _, [] -> raise (Invalid_argument
   "zip: unequal length lists")
   | xhd :: xtl, yhd :: ytl ->
     Metered.cons (Metered.pair xhd yhd) (zip xtl ytl) ;;


     (* open Metered ;; *)
     let rec zip (xs : 'a list)
        (ys : 'b list)
       : ('a * 'b) list =
       match xs, ys with
       | [],[] -> []
       | [],_
       | _, [] -> raise (Invalid_argument
                           "zip: unequal length lists")
       | xhd :: xtl, yhd :: ytl ->
         Metered.cons (Metered.pair xhd yhd) (zip xtl ytl) ;;
