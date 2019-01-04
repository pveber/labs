open Bistro
open Bistro_engine

let np = ref 8
let mem = ref 8

let with_workflow w ~f =
  let open Scheduler in
  let db = Db.init_exn "_bistro" in
  let sched = create ~np:!np ~mem:(`GB !mem) db in
  let thread = eval_exn sched w in
  start sched ;
  Lwt_main.run thread
  |> f

let with_pworkflow w ~f = with_workflow (Workflow.eval_path w) ~f

let path w =
  with_pworkflow w ~f:(fun x -> x)

let sh fmt =
  Printf.kprintf (fun s -> ignore (Sys.command s)) fmt

let rm w =
  with_pworkflow w ~f:(fun p ->
      sh "rm %s" p
    )

let less (w : #text_file pworkflow) =
  with_pworkflow w ~f:(fun p ->
      sh "less %s" p
    )

let browse w =
  with_pworkflow w ~f:(fun p ->
      sh "firefox --no-remote %s" p
    )

let ls (w : #directory pworkflow) =
  with_pworkflow w ~f:(fun p ->
      sh "ls %s" p
    )
