open Base
open Biocaml_base
include Pipes_unix.Pipe

let ( %% ) f g = Fn.(flip compose) f g
let ( >>= ) = bind

let ok_or_failwith = function
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg

let lines () =
  let open Lines.Parser in
  loop step initial_state

let lines_to_strings () =
  let rec loop () =
    await () >>= function
    | None -> return ()
    | Some (l : Line.t) ->
      yield (l :> string) >>= fun () ->
      yield "\n" >>= fun () ->
      loop ()
  in
  loop ()

let bed_parser () =
  lines ()
  $$ map (Bed.item_of_line %% Result.ok_or_failwith)

let gff3_parser () =
  lines ()
  $$ map (Gff.gff3_item_of_line %% ok_or_failwith)


let gff_unparser version =
  map (Gff.line_of_item version)
  $$ lines_to_strings ()
