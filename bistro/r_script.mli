open Bistro

type t
type expr
type arg


val make : expr list -> Template.t

val dest : expr
val tmp : expr
val source : string -> expr
val call : string -> arg list -> expr
val string : string -> expr
val int : int -> expr
val float : float -> expr
val dep : _ workflow -> expr
val floats : float list -> expr
val strings : string list -> expr
val deps : _ workflow list -> expr
val arg : ?l:string -> expr -> arg
val assign : string -> expr -> expr
