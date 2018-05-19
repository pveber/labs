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
  method time : time
  method event  : event -> unit
  method stop : unit
  method running : bool
  (* method wait4shutdown : unit Lwt.t *)
end

class virtual base = object (s)
  val mutable running = true
  method time = Unix.gettimeofday ()
  method stop =
    running <- false
  method running = running
end

let null : t = object (s)
  inherit base
  method event _ = ()
end
