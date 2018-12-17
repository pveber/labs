open Bistro
open Bistro.Template_dsl

type t
type expr
type arg


val make : expr list -> template

val dest : expr
val tmp : expr
val source : string -> expr
val call : string -> arg list -> expr
val string : string -> expr
val int : int -> expr
val float : float -> expr
val dep : _ pworkflow -> expr
val ints : int list -> expr
val floats : float list -> expr
val strings : string list -> expr
val deps : _ pworkflow list -> expr
val arg : ?l:string -> expr -> arg
val assign : string -> expr -> expr

val workflow :
  ?descr:string ->
  ?np:int ->
  ?mem:int workflow ->
  ?env:Shell_dsl.docker_image ->
  expr list ->
  'a pworkflow
