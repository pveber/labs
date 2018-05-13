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
  db:Db.t ->
  t

val stats : t -> stats

val submit :
  t ->
  _ Workflow.t ->
  unit
