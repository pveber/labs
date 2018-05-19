type event =
  (* | Init of { dag : t ; needed : task list ; already_done : task list } *)
  | Task_ready of Task.t
  | Task_started of Task.t * Allocator.resource
  | Task_ended of Task.t
  | Task_skipped of Task.t * [ `Done_already
                             | `Missing_dep
                             | `Allocation_error of string ]

type time = float

class type t = object
  method event : event -> unit
  method time : time
  method running : bool
  method stop : unit
  (* method wait4shutdown : unit Lwt.t *)
end


class virtual base : object
  method time : float
  method stop : unit
  method running : bool
end

val null : t

