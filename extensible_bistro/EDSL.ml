open Core_kernel

type 'a workflow = 'a Workflow.t

type 'a path = 'a Workflow.path
include Path_types

type 'a file = (#file_format as 'a) path workflow
type 'a directory = 'a directory_format path workflow

type docker_image = Command.docker_image

let docker_image ?tag ?registry ~account ~name () = Command.{
  dck_account = account ;
  dck_name = name ;
  dck_tag = tag ;
  dck_registry = registry ;
}


module Sh = struct
  open Template
  open Command

  type fragment = Workflow.path_dep Template.t
  type command = Workflow.path_dep Command.t

  let dest = [ DEST ]
  let tmp = [ TMP ]
  let np = [ NP ]
  let mem = [ MEM ]
  let exe = [ EXE ]

  let string s = [ S s ]
  let int i = string (string_of_int i)
  let float f = string (Float.to_string f)
  let dep w = [ D (Workflow.Path_dep w) ]

  let quote ?using:(c = '"') e =
    let quote_symbol = S (Char.to_string c) in
    quote_symbol :: e @ [ quote_symbol ]

  let option f = function
    | None -> []
    | Some x -> f x

  let list f ?(sep = ",") l =
    List.map l ~f
    |> List.intersperse ~sep:(string sep)
    |> List.concat

  let seq ?sep xs =
    let format = match sep with
      | None -> ident
      | Some sep -> List.intersperse ~sep:(string sep)
    in
    List.concat (format xs)

  let enum dic x = string (List.Assoc.find_exn ~equal:( = ) dic x)

  let file_dump contents = [ F contents ] (* FIXME: should check that there is no file_dump in contents *)

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
      |> List.intersperse ~sep:(string " ")
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

  let mkdir_p d = cmd "mkdir" [ string "-p" ; d ]

  let cd p = cmd "cd" [ p ]

  let rm_rf x = cmd "rm" [ string "-rf" ; x ]

  let mv x y = cmd "mv" [ x ; y ]

  let ( // ) x y = Template.(x @ [ S "/" ; S y ])

  let or_list xs = Or_list xs
  let and_list xs = And_list xs
  let pipe xs = Pipe_list xs

  let ( % ) f g x = g (f x)
end

let shell ?descr ?mem ?np ?version cmds =
  Workflow.shell ?descr ?mem ?np ?version (Sh.and_list cmds)

let input = Workflow.input
let select = Workflow.select
