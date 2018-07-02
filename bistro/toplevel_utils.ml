open Bistro

let with_workflow w ~f =
  eval_expr_exn ~np:8 ~mem:(`GB 8) Expr.(dep (pureW w))
  |> f

let path w =
  with_workflow w ~f:(fun x -> x)

let sh fmt =
  Printf.kprintf (fun s -> ignore (Sys.command s)) fmt

let rm w =
  with_workflow w ~f:(fun p ->
      sh "rm %s" p
    )

let less (w : #text_file workflow) =
  with_workflow w ~f:(fun p ->
      sh "less %s" p
    )

let browse w =
  with_workflow w ~f:(fun p ->
      sh "firefox --no-remote %s" p
    )

let ls (w : _ #directory workflow) =
  with_workflow w ~f:(fun p ->
      sh "ls %s" p
    )
