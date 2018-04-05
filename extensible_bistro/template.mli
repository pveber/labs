type 'a token =
  | S of string
  | D of 'a
  | F of 'a t (* Fragment *)
  | DEST
  | TMP
  | NP
  | MEM
  | EXE

and 'a t = 'a token list

val map : 'a t -> f:('a -> 'b) -> 'b t

val deps : 'a t -> 'a list

module EDSL : sig
  val dest : _ t
  (** Symbol representing the location where a workflow is expected to
      produce its result *)

  val tmp : _ t
  (** Symbol representing an existing empty directory that can be used
      as a temporary space for a workflow's execution. *)

  val np : _ t
  (** Symbol representing the number of cores allocated to the
      workflow *)

  val mem : _ t
  (** Symbol representing the memory size allocated to the workflow,
      in GB. *)

  val exe : _ t
  (** Symbol representing the path of the current executable, as
      specified by [Sys.argv.(0)] *)

  val string : string -> _ t
  (** A chunk of text *)

  val int : int -> _ t
  (** Int formatting *)

  val float : float -> _ t
  (** Float formatting *)

  val dep : 'dep -> 'dep t
  (** [dep w] is interpreted as the path where to find the result of
      workflow [w] *)

  val quote : ?using:char -> 'a t -> 'a t
  (** [quote ~using:c t] surrounds template [t] with character [c] *)

  val option : ('a -> 'b t) -> 'a option -> 'b t
  (** [option f o] is [f x] if [o = Some x] and [string ""]
      otherwise *)

  val list : ('a -> 'b t) -> ?sep:string -> 'a list -> 'b t
  (** list combinator, optional value of [sep] is [","] *)

  val seq : ?sep:string -> 'a t list -> 'a t
  (** another list combinator, default value for [sep] is [""] *)

  val enum : ('a * string) list -> 'a -> _ t
  (** combinator for enumerations *)

  val file_dump : 'a t -> 'a t
  (** [file_dump t] can be used when a command needs a configuration
      script: at run-time, it will generate a text using [t], save it
      to a path, deterministically chosen as a function of
      [t]. Finally the template [file_dump t] is interpreted as this
      path. *)
end

include module type of EDSL
