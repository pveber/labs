open Core

include Template.EDSL

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

let rec map ~f = function
  | Docker (im, cmd) -> Docker (im, map ~f cmd)
  | Simple_command toks ->
    Simple_command (Template.map ~f toks)
  | And_list cmds -> And_list (List.map cmds ~f:(map ~f))
  | Or_list cmds -> Or_list (List.map cmds ~f:(map ~f))
  | Pipe_list cmds -> Pipe_list (List.map cmds ~f:(map ~f))

let docker image cmd = Docker (image, cmd)

let gen_cmd prog_expr ?env ?stdin ?stdout ?stderr args =
  let stdout_expr =
    match stdout with
    | None -> []
    | Some e -> Template.S " > " :: e
  in
  let stdin_expr =
    match stdin with
    | None -> []
    | Some e -> Template.S " < " :: e
  in
  let stderr_expr =
    match stderr with
    | None -> []
    | Some e -> Template.S " 2> " :: e
  in
  let tokens =
    [ prog_expr ] @ args @ [ stdin_expr ; stdout_expr ; stderr_expr ]
    |> List.filter ~f:(( <> ) [])
    |> List.intersperse ~sep:(Template.string " ")
    |> List.concat
  in
  let cmd = Simple_command tokens in
  match env with
  | None -> cmd
  | Some image -> docker image cmd

let cmd p = gen_cmd [ S p ]

let internal_cmd subcmd = gen_cmd [ EXE ; S " " ; S subcmd ] ?env:None

let opt o f x = Template.(S o :: S " " :: f x)

let opt' o f x = Template.(S o :: S "=" :: f x)

let flag f x b = if b then f x else []

let mkdir d = cmd "mkdir" [ d ]

let mkdir_p d = cmd "mkdir" [ Template.string "-p" ; d ]

let cd p = cmd "cd" [ p ]

let rm_rf x = cmd "rm" [ Template.string "-rf" ; x ]

let mv x y = cmd "mv" [ x ; y ]

let ( // ) x y = Template.(x @ [ S "/" ; S y ])

let or_list xs = Or_list xs
let and_list xs = And_list xs
let pipe xs = Pipe_list xs

let docker_image ?tag ?registry ~account ~name () = {
  dck_account = account ;
  dck_name = name ;
  dck_tag = tag ;
  dck_registry = registry ;
}

let ( % ) f g x = g (f x)
