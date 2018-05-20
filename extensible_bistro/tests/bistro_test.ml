open Core
open Labs_extensible_bistro.EDSL
open Labs_extensible_bistro_engine

let stanford_parser_env =
  docker_image ~account:"pveber" ~name:"stanford-parser" ~tag:"3.9.1" ()

let stanford_parser (txt : #text_encoded file) : text_encoded file =
  shell ~descr:"stanford-parser" Sh.[
      cmd "lexparser.sh" ~stdout:dest [ dep txt ]
    ]

let waffles : text_encoded file = input "test/waffles.txt"

let goal = stanford_parser waffles

let db = Db.init_exn "_bistro"
let sched = Scheduler.create ~db ()

let () =
  let open Scheduler in
  submit sched goal ;
  stats sched
  |> sexp_of_stats
  |> Sexp.output_hum stdout ;
  Out_channel.output_char stdout '\n' ;
  Out_channel.flush stdout ;
  let t = eval sched goal in
  start sched ;
  Lwt_main.run t
  |> (fun (Path p) -> print_endline p)
