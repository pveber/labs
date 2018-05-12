open Core_kernel

type 'a path = Path of string

type _ t =
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

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

let rec id : type s. s t -> string = function
  | Input { id } -> id
  | Shell { id } -> id
  | Select { id } -> id
  | Glob { id } -> id
  | Map { id } -> id
  | Value { id } -> id
  | Const { id } -> id
  | List { id } -> id

let digestible_command c =
  Command.map c ~f:(function (Path_dep d) -> id d)

let digestible_deps xs =
  List.map xs ~f:(function (Path_dep d) -> id d)

let shell ?(descr = "") ?(mem = 100) ?(np = 1) ?version cmd =
  let deps = Command.deps cmd in
  let id = digest ("shell",
                   version,
                   digestible_command cmd, digestible_deps deps) in
  Shell {
    id ;
    descr ;
    mem ;
    np ;
    cmd ;
  }

let select_id dir path = digest ("select", id dir, path)

let normalize_select : type s. s t -> s t = function
  | Select {
      dir = Select { dir = dir1 ; path = p1 } ;
      path = p2 ;
    } ->
    let dir = dir1 in
    let path = p1 @ p2 in
    Select {
      id = select_id dir path ;
      dir ;
      path ;
    }
  | x -> x

let select dir path =
  Select {
    id = select_id dir path ;
    dir ;
    path ;
  }
  |> normalize_select

let input ?(may_change = false) target =
  let hash =
    if may_change then
      Some (Md5.digest_file_blocking_without_releasing_runtime_lock target)
    else None in
  let id = digest ("input", target, hash) in
  Input { id ; path = Lpath.make_relative target }
