open Core
open Lwt
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

type task_entry = {
  id : string ;
  task : Task.t ;
  outcome : Task.Outcome.t Lwt.t ;
  send_outcome : Task.Outcome.t Lwt.u ;
  mutable status : [`W4DEPS | `READY | `RUNNING | `DONE | `FAILED] ;
}

let task_entry w task status =
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
  alloc : Allocator.t ;
  logger : Logger.t ;
  tasks : task_entry Table.t ;
  mutable running : bool ;
  start_signal : unit Lwt_condition.t ;
}

let stats sched =
  Table.fold sched.tasks ~init:zero_stats ~f:(fun ~key:_ ~data:ts acc ->
      incr_stats acc ts.status
    )

let create ?(logger = Logger.null) ?(np = 1) ?(mem = `MB 100) ~db () =
  let `MB mem = mem in
  {
    db ;
    alloc = Allocator.create ~np ~mem ;
    tasks = Table.create () ;
    logger ;
    running = false ;
    start_signal = Lwt_condition.create () ;
  }

let add_task sched w task status =
  let entry = task_entry w task status in
  Table.set sched.tasks (Task.id task) entry

let wait4start sched =
  if sched.running then Lwt.return ()
  else Lwt_condition.wait sched.start_signal

let dep_id (Workflow.Dep x) = Workflow.id x

let wait4deps sched deps =
  let check_outcome (Workflow.Dep d) =
    match Table.find sched.tasks (Workflow.id d) with
    | Some ts -> ts.outcome
    | None ->
      (* never call [deps_completed] before having registered the deps *)
      assert false
  in
  Lwt_list.map_p check_outcome deps >|= fun xs ->
  if List.for_all xs ~f:Task.Outcome.succeeded then Ok ()
  else Error `Some_dep_failed

let deps_of_command cmd =
  Command.deps cmd
  |> List.map ~f:(fun (Workflow.Path_dep w) -> Workflow.Dep w)

let shell_task_thread sched w ~id ~descr ~cmd ~np ~mem ~deps =
  let task = Task.of_shell sched.db ~id ~cmd ~descr ~np ~mem in (* FIXME *)
  let status = task_entry w task `W4DEPS in
  Table.set sched.tasks id status ;
  wait4deps sched deps >>= function
  | Error `Some_dep_failed ->
    status.status <- `FAILED ;
    Lwt_result.fail `Some_dep_failed
  | Ok () ->
    Allocator.request sched.alloc (Task.requirement task) >>= function
    | Ok resource ->
      sched.logger#event (Task_started (task, resource)) ;
      Task.perform task >>= fun outcome ->
      Allocator.release sched.alloc resource ;
      Lwt.return outcome
    | Error (`Msg msg) ->
      let err = `Allocation_error msg in
      sched.logger#event (Task_skipped (task, err)) ;
      Lwt_result.fail (`Skipped err)

let rec register : type s. t -> s Workflow.t -> unit = fun sched w ->
  let is_registered = Table.mem sched.tasks (Workflow.id w) in
  match w with
  | Workflow.Input { id ; path } ->
    if not is_registered then (
      add_task sched w (Task.of_input ~id) `READY
    )

  | Workflow.Shell { id ; cmd ; mem ; np ; descr } ->
    if not is_registered then (
      let task = Task.of_shell sched.db ~id ~descr ~np ~mem ~cmd in
      let is_done = Db.cache_mem sched.db w in
      let status = if is_done then `DONE else `W4DEPS in
      let () =
        if not is_done then (
          let deps = deps_of_command cmd in
          List.iter deps ~f:(fun (Workflow.Dep w) -> register sched w)
        )
      in
      add_task sched w task status
    )

  | _ -> assert false


let submit sched w =
  register sched w

let rec eval : type s. t -> s Workflow.t -> s Lwt.t = fun sched w ->
  let open Workflow in
  register sched w ;
  match w with
  | Const x -> Lwt.return x.value
  | List ws ->
    Lwt_list.map_p (eval sched) ws.value
  | Input { id ; path } ->
    eval_aux sched id >|= fun () ->
    Path (Lpath.to_string path)
  | Shell { id } ->
    eval_aux sched id >|= fun () ->
    Path (Db.cache sched.db id)
  | Select { id ; dir ; path = path_in_dir} ->
    eval_aux sched id >>= fun () ->
    eval sched dir >>= fun (Path p) ->
    Lwt.return (Path (Filename.concat p (Lpath.to_string path_in_dir)))
  | Glob _ -> assert false
  | Map _ -> assert false
  | Value _ -> assert false

and eval_aux sched id =
  let status = Table.find_exn sched.tasks id in (* cannot fail because we called register above *)
  let%lwt outcome = status.outcome in
  if Task.Outcome.succeeded outcome then
    Lwt.return ()
  else
    Lwt.fail_with (sprintf "error while evaluating %s" id)

let start sched =
  sched.running <- true ;
  Lwt_condition.broadcast sched.start_signal ()
