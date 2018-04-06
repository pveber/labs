include module type of Template.EDSL

type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type 'a t =
  | Docker of docker_image * 'a t
  | Simple_command of 'a Template.t
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

val cmd :
  string ->
  ?env:docker_image ->
  ?stdin:'a Template.t -> ?stdout:'a Template.t -> ?stderr:'a Template.t ->
  'a Template.t list -> 'a t
(** Command-line constructor, e.g. [cmd "echo" ~stdout:dest [ string
    "foo" ]] will generate a shell command like ["echo foo >
    /some/path"].
    - @param env specifies a Docker image where to run the command
    - @param stdin adds a ["< /some/path"] token at the end of the command
    - @param stdout adds a ["> /some/path"] token at the end of the command
    - @param stderr adds a ["2> /some/path"] token at the end of the command *)

val opt : string -> ('a -> 'b Template.t) -> 'a -> 'b Template.t
(** Command-line option formatting, e.g.: [opt "--output" dep dest]
    will be rendered like ["--output /some/path"] *)

val opt' : string -> ('a -> 'b Template.t) -> 'a -> 'b Template.t
(** Same as {!val:opt} but renders options with an equal sign,
    e.g. ["--output=/some/path"] *)

val flag : ('a -> 'b Template.t) -> 'a -> bool -> 'b Template.t
(** [flag f x b] renders as [f x] if [b] is true *)

val or_list : 'a t list -> 'a t
(** OR-sequence of commands ([ || ]) *)

val and_list : 'a t list -> 'a t
(** AND-sequence of commands ([ && ]) *)

val pipe : 'a t list -> 'a t
(** Pipe of commands ([ | ]) *)

val ( // ) : 'a Template.t -> string -> 'a Template.t
(** Similar to {!val:Filename.concat}, but with other types. *)

(** {5 Useful commands} *)

val mkdir : 'a Template.t -> 'a t
val mkdir_p : 'a Template.t -> 'a t
val cd : 'a Template.t -> 'a t
val rm_rf : 'a Template.t -> 'a t
val mv : 'a Template.t -> 'a Template.t -> 'a t

(** {5 Docker-related} *)

val docker_image :
  ?tag:string ->
  ?registry:string ->
  account:string ->
  name:string ->
  unit -> docker_image
(** Construct a description of a publicly available docker image *)

val docker : docker_image -> 'a t -> 'a t
(** [docker cmd] transforms [cmd] so that it can be executed in a
    Docker container. *)

val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
