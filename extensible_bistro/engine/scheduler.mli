open Labs_extensible_bistro

type t

type stats = {
  goals : counts ;
  steps : counts ;
}
and counts = {
  w4deps  : int ;
  ready   : int ;
  running : int ;
  failed  : int ;
  _done_  : int ;
}
[@@deriving sexp]

val create :
  ?logger:Logger.t ->
  ?np:int ->
  ?mem:[`MB of int] ->
  db:Db.t ->
  unit -> t

val stats : t -> stats

val submit :
  t ->
  _ Workflow.t ->
  unit

val eval :
  t ->
  'a Workflow.t ->
  'a Lwt.t

val start : t -> unit
