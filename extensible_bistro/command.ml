open Core

type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type 'a t =
  | Docker of docker_image * 'a t
  | Simple_command of 'a Template.t
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list


let rec deps = function
  | And_list xs
  | Or_list xs
  | Pipe_list xs ->
    List.map xs ~f:deps
    |> List.concat
    |> List.dedup_and_sort ~compare:Pervasives.compare
  | Simple_command tokens -> Template.deps tokens
  | Docker (_, c) -> deps c

let rec map c ~f = match c with
  | Docker (im, cmd) -> Docker (im, map ~f cmd)
  | Simple_command toks ->
    Simple_command (Template.map ~f toks)
  | And_list cmds -> And_list (List.map cmds ~f:(map ~f))
  | Or_list cmds -> Or_list (List.map cmds ~f:(map ~f))
  | Pipe_list cmds -> Pipe_list (List.map cmds ~f:(map ~f))

