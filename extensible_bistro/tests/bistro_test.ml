open Core
open Labs_extensible_bistro.EDSL
open Labs_extensible_bistro_engine

let text_file : text_encoded file = input "test/waffles.txt"

let db = Db.init_exn "_bistro"
let sched = Scheduler.create ~db
let () =
  let open Scheduler in
  submit sched text_file ;
  stats sched
  |> sexp_of_stats
  |> Sexp.output_hum stdout ;
  Out_channel.output_char stdout '\n'
