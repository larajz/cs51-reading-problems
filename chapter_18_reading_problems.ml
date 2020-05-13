class type counter_interface =
  object
    method bump : int -> unit
    method get_state : int
  end ;;

class counter : counter_interface =
  object
    val mutable state = 0
    method bump n = state <- state + n
    method get_state = state
end ;;

class loud_counter : counter_interface =
  object
    val mutable state = 0
    method bump n = state <- state + n; Printf.printf " %d " n
    method get_state = state
  end ;;

class loud_counter : counter_interface =
  object(this)
    inherit counter as co
    method! bump n = co#bump n; Printf.printf " %d " this#get_state
  end ;;

  (* class loud_counter : counter_interface =
  object (this)
  inherit counter as super
  method! bump n =
  super#bump n;
  Printf.printf "State is now %d \n" this#get_state
  end ;; *)

class type reset_counter_interface =
  object
    inherit counter_interface
    method reset : unit
  end ;;

class loud_reset_counter : reset_counter_interface =
  object(this)
    inherit loud_counter
    method reset = this#bump (-this#get_state)
  end ;;

  (* class loud_reset_counter : reset_counter_interface =
  object(this)
    inherit loud_counter as super
    method reset = this#bump (-this#get_state)
    end ;; *)
