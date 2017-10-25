open Base
open Bistro

type t = Template.t

type expr = Template.t

type arg = Template.t

let make xs = Template.(seq ~sep:"\n" xs)

let source s = Template.string s

let dest = Template.(quote ~using:'"' dest)

let tmp = Template.(quote ~using:'"' tmp)

let string s =
  Template.(quote ~using:'"' (string s))

let int i = Template.int i

let float f = Template.float f

let dep w = Template.(quote ~using:'"' (dep w))

let call_gen fn arg xs  =
  let open Template in
  seq ~sep:"" [
    string fn ;
    string "(" ;
    list ~sep:"," arg xs ;
    string ")" ;
  ]

let call fn args = call_gen fn Fn.id args

let vector f xs = call_gen "c" f xs

let ints xs = vector Template.int xs
let floats xs = vector Template.float xs
let strings xs = vector string xs
let deps xs = vector dep xs

let arg ?l e =
  let open Template in
  match l with
  | None -> e
  | Some label ->
    seq ~sep:"" [ string label ; string "=" ; e ]

let assign var e =
  let open Template in
  seq ~sep:" " [ string var ; string "<-" ; e ]

let workflow ?descr ?np ?mem ?env exprs =
  EDSL.workflow ?descr ?np ?mem EDSL.[
    cmd "Rscript" [ file_dump (make exprs) ] ;
  ]
