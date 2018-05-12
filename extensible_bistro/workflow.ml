type 'a path = Path of string

type _ t = private
  | Const : 'a -> 'a t
  | List : 'a t list -> 'a list t
  | Input : {
      id : string ;
      path : string ;
    } -> 'a path t
  | Command : {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      cmd : path_dep Command.t
    } -> 'a path t
  | Select : {
      id : string ;
      dir : < path_kind : [`Directory] ; .. > path t ;
      path : string list ;
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

and path_dep = Path_dep : _ path t -> path_dep
