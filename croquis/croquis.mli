open Gg
open Vg

module Viewport : sig
  type t

  val linear :
    xlim:float * float ->
    ylim:float * float ->
    size:float * float ->
    t

  val scale_x : t -> float -> float
  val scale_y : t -> float -> float
  val scale : t -> v2 -> v2
end

class type t = object
  method image : image
  method bbox : Box2.t
end

type arrow_style = [
  | `none
  | `triangle
]

class path :
  ?vp:Viewport.t ->
  ?col:Color.t ->
  ?tip:arrow_style ->
  v2 -> v2 list ->
  object
    inherit t
    method start : v2
    method _end_ : v2
  end

class venn_diagram2 :
  ?a_label:string ->
  ?b_label:string ->
  compare:('a -> 'a -> int) ->
  a:'a list ->
  b:'a list ->
  unit -> t

val render :
  t ->
  string ->
  unit

val demo : unit -> unit
