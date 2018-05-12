type 'a path = Path of string

type _ t = private
  | Const : {
      id : string ;
      value : 'a ;
    } -> 'a t
  | List : {
      id : string ;
      value : 'a t list ;
    } -> 'a list t
  | Input : {
      id : string ;
      path : Lpath.t ;
    } -> 'a path t
  | Shell : {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      cmd : path_dep Command.t
    } -> 'a path t
  | Select : {
      id : string ;
      dir : < path_kind : [`Directory] ; .. > path t ;
      path : Lpath.t ;
    } -> 'a path t
  | Glob : {
      id : string ;
      dir : < path_kind : [`Directory] ; .. > path t ;
      pattern : string ;
    } -> 'a path list t
  | Map : {
      id : string ;
      elts : 'a list t ;
      f : 'a t -> 'b t ;
    } -> 'b list t
  | Value : {
      id : string ;
      deps : dep list ;
      build : env -> string -> unit ;
      load : string -> 'a ;
    } -> 'a t

and path_dep = Path_dep : _ path t -> path_dep
and dep = Dep : _ t -> dep

and env = <
  dep : 'a. 'a t -> 'a ;
  np : int ;
  mem : int ;
  tmp : string ;
  dest : string
>

val id : _ t -> string

val input : ?may_change:bool -> string -> 'a path t

val shell :
  ?descr:string ->
  ?mem:int ->
  ?np:int ->
  ?version:int ->
  path_dep Command.t -> 'a path t

val select :
  < path_kind : [`Directory] ; .. > path t ->
  string list ->
  'a path t
(** Selector constructor *)
