(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)
open Labs_extensible_bistro.EDSL

type nonrec 'a result = ('a, [`Msg of string]) result

type t
(** An abstract type for databases *)

val init : string -> t result
(** [init path] creates a database located at path [path], which can
    be absolute or relative. If the path already exists, its contents
    is inspected to see if it looks like a bistro database; if not, a
    fresh database is created on the filesystem.

    Returns an error message if [path] is occupied with something else
    than a bistro database. *)

val init_exn : string -> t
(** @raise Failure*)

val tmp_dir : t -> string
val cache_dir : t -> string
val stdout_dir : t -> string
val stderr_dir : t -> string
val build_dir : t -> string

val tmp : t -> string -> string
val cache : t -> string -> string
val stdout : t -> string -> string
val stderr : t -> string -> string
val build : t -> string -> string

val workflow_path : t -> _ path workflow -> string
val cache_mem : t -> _ path workflow -> bool
