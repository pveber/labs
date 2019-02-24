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

type thickness = [
  | `normal
  | `thick
]

val path :
  ?vp:Viewport.t ->
  ?col:Color.t ->
  ?tip:arrow_style ->
  v2 -> v2 list ->
  < t ;
    start : v2 ;
    _end_ : v2 >

val rectangle :
  ?vp:Viewport.t ->
  ?col:Color.t ->
  ?thickness:thickness ->
  center:v2 ->
  size:v2 ->
  unit ->
  t

val rectangle' :
  ?vp:Viewport.t ->
  ?col:Color.t ->
  ?thickness:thickness ->
  xmin:float ->
  xmax:float ->
  ymin:float ->
  ymax:float ->
  unit ->
  t

val venn_diagram2 :
  compare:('a -> 'a -> int) ->
  a:'a list ->
  b:'a list ->
  unit -> t

val render :
  t ->
  string ->
  unit

val demo : unit -> unit
