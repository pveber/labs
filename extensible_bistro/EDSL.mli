type 'a workflow = 'a Workflow.t

type 'a path = 'a Workflow.path
include module type of Path_types

type 'a file = (#file_format as 'a) path workflow
type 'a directory = 'a directory_format path workflow

type docker_image

val docker_image :
  ?tag:string ->
  ?registry:string ->
  account:string ->
  name:string ->
  unit -> docker_image
(** Construct a description of a publicly available docker image *)

module Sh : sig

  (** FRAGMENTS *)

  type fragment

  val dest : fragment
  (** Symbol representing the location where a workflow is expected to
      produce its result *)

  val tmp : fragment
  (** Symbol representing an existing empty directory that can be used
      as a temporary space for a workflow's execution. *)

  val np : fragment
  (** Symbol representing the number of cores allocated to the
      workflow *)

  val mem : fragment
  (** Symbol representing the memory size allocated to the workflow,
      in GB. *)

  val exe : fragment
  (** Symbol representing the path of the current executable, as
      specified by [Sys.argv.(0)] *)

  val string : string -> fragment
  (** A chunk of text *)

  val int : int -> fragment
  (** Int formatting *)

  val float : float -> fragment
  (** Float formatting *)

  val dep : _ path workflow -> fragment
  (** [dep w] is interpreted as the path where to find the result of
      workflow [w] *)

  val quote : ?using:char -> fragment -> fragment
  (** [quote ~using:c t] surrounds template [t] with character [c] *)

  val option : ('a -> fragment) -> 'a option -> fragment
  (** [option f o] is [f x] if [o = Some x] and [string ""]
      otherwise *)

  val list : ('a -> fragment) -> ?sep:string -> 'a list -> fragment
  (** list combinator, optional value of [sep] is [","] *)

  val seq : ?sep:string -> fragment list -> fragment
  (** another list combinator, default value for [sep] is [""] *)

  val enum : ('a * string) list -> 'a -> fragment
  (** combinator for enumerations *)

  val file_dump : fragment -> fragment
  (** [file_dump t] can be used when a command needs a configuration
      script: at run-time, it will generate a text using [t], save it
      to a path, deterministically chosen as a function of
      [t]. Finally the template [file_dump t] is interpreted as this
      path. *)

  type command

  val cmd :
    string ->
    ?env:docker_image ->
    ?stdin:fragment -> ?stdout:fragment -> ?stderr:fragment ->
    fragment list -> command
  (** Command-line constructor, e.g. [cmd "echo" ~stdout:dest [ string
      "foo" ]] will generate a shell command like ["echo foo >
      /some/path"].
      - @param env specifies a Docker image where to run the command
      - @param stdin adds a ["< /some/path"] token at the end of the command
      - @param stdout adds a ["> /some/path"] token at the end of the command
      - @param stderr adds a ["2> /some/path"] token at the end of the command *)

  val opt : string -> ('a -> fragment) -> 'a -> fragment
  (** Command-line option formatting, e.g.: [opt "--output" dep dest]
      will be rendered like ["--output /some/path"] *)

  val opt' : string -> ('a -> fragment) -> 'a -> fragment
  (** Same as {!val:opt} but renders options with an equal sign,
      e.g. ["--output=/some/path"] *)

  val flag : ('a -> fragment) -> 'a -> bool -> fragment
  (** [flag f x b] renders as [f x] if [b] is true *)

  val or_list : command list -> command
  (** OR-sequence of commands ([ || ]) *)

  val and_list : command list -> command
  (** AND-sequence of commands ([ && ]) *)

  val pipe : command list -> command
  (** Pipe of commands ([ | ]) *)

  val ( // ) : fragment -> string -> fragment
  (** Similar to {!val:Filename.concat}, but with other types. *)

  (** {5 Useful commands} *)

  val mkdir : fragment -> command
  val mkdir_p : fragment -> command
  val cd : fragment -> command
  val rm_rf : fragment -> command
  val mv : fragment -> fragment -> command

  val docker : docker_image -> command -> command
  (** [docker cmd] transforms [cmd] so that it can be executed in a
      Docker container. *)

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

end


val shell :
  ?descr:string ->
  ?mem:int ->
  ?np:int ->
  ?version:int ->
  Sh.command list -> 'a path workflow
(** Workflow constructor, taking a list of commands in input. Other arguments are:
    - @param descr description of the workflow, used for logging
    - @param mem required memory
    - @param np maximum number of cores (could be given less at execution)
    - @param version version number, used to force the rebuild of a workflow *)

val input : ?may_change:bool -> string -> 'a path workflow
(** Constructs a workflow from an existing file on the
    filesystem. The argument [may_change] indicates that the file
    may be modified, which is detected by giving the workflow a
    digest of the file as an input. *)

val select : 'a directory -> string list -> 'a path workflow
(** Selector constructor *)
