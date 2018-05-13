open Core
open Labs_extensible_bistro

module Table = String.Table

type stats = {
  goals : counts ;
  steps : counts ;
}
and counts = {
  w4deps  : int ;
  ready   : int ;
  running : int ;
  failed  : int ;
  _done_  : int ;
}
[@@deriving sexp]

let zero_counts =
  { w4deps = 0 ; ready = 0 ; running = 0 ; failed = 0 ; _done_ = 0 }

let zero_stats = {
  goals = zero_counts ;
  steps = zero_counts ;
}

let incr_counts c = function
  | `W4DEPS -> { c with w4deps = c.w4deps + 1 }
  | `READY -> { c with ready = c.ready + 1 }
  | `RUNNING -> { c with running = c.running + 1 }
  | `FAILED -> { c with failed = c.failed + 1 }
  | `DONE -> { c with _done_ = c._done_ + 1 }

let incr_stats stats status =
  { stats with steps = incr_counts stats.steps status }

type outcome = Succeeded | Failed

type task_status = {
  id : string ;
  task : Task.t ;
  outcome : outcome Lwt.t ;
  send_outcome : outcome Lwt.u ;
  mutable status : [`W4DEPS | `READY | `RUNNING | `DONE | `FAILED] ;
}

let task_status w task status =
  let outcome, send_outcome = Lwt.wait () in
  {
    id = Workflow.id w ;
    task ;
    outcome ;
    send_outcome ;
    status ;
  }

type t = {
  db : Db.t ;
  tasks : task_status Table.t ;
}

let stats sched =
  Table.fold sched.tasks ~init:zero_stats ~f:(fun ~key:_ ~data:ts acc ->
      incr_stats acc ts.status
    )

let create ~db = {
  db ;
  tasks = Table.create () ;
}

let rec register : type s. t -> s Workflow.t -> unit = fun sched w ->
  if not (Table.mem sched.tasks (Workflow.id w)) then (
    match w with
    | Workflow.Input { id ; path } ->
      let status = task_status w Task.Input `READY in
      Table.set sched.tasks id status
    | _ -> assert false
  )

let submit sched w =
  register sched w
