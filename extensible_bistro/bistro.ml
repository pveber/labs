open Core_kernel

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

type 'a path = Path of string

class type file = object
  method kind : [`File]
end

class type binary_file = object
  inherit file
  method encoding : [`Binary_encoding]
end

class type text_file = object
  inherit file
  method encoding : [`Text_encoding]
end

class type html = object
  inherit file
  method format : [`Html]
end

class type ['a] ocaml_value = object
  inherit binary_file
  method load : 'a
end

type bbool = bool ocaml_value
type bstring = string ocaml_value
type 'a blist = 'a list ocaml_value

module type Lang = sig
  type 'a t
  type 'a pt = 'a path t

  val string : string -> string t
  val list : 'a t list -> 'a list t
  val input : string -> _ path t
  val load : 'a ocaml_value path t -> 'a t
  val map : 'a list t -> f:('a t -> 'b t) -> 'b list t
  val _if_ : bool t -> 'a t -> 'a t -> 'a t

  module Unix_utils : sig
    val wget :
      ?no_check_certificate:bool ->
      ?user:string ->
      ?password:string ->
      string t -> _ path t
    val grep : string -> #text_file pt -> #text_file pt
  end

  module Text_file : sig
    val lines : #text_file pt -> string list t
  end
end


module NLP_wikipedia(L : Lang) = struct
  open L

  let protein_wikipedia_page =
    string "https://en.wikipedia.org/wiki/Protein"
    |> Unix_utils.wget

  let links =
    Unix_utils.grep "https://en.wikipedia.org/wiki/[^ ]+" protein_wikipedia_page
    |> Text_file.lines

  let pages : html path list t =
    map links ~f:Unix_utils.wget
end

module Engine = struct
  type 'a t = ..

  type 'a pt = 'a path t

  type any = Any : _ t -> any

  type _ t +=
    | Const : 'a -> 'a t
    | List  : 'a t list -> 'a list t
    | Input : string -> 'a path t
    | Load_value : 'a ocaml_value path t -> 'a t
    | If : { cond : bool t ; _then_ : 'a t ; _else_ : 'a t } -> 'a t
    | Map : 'a list t * ('a t -> 'b t) -> 'b list t
    | Command : any Command.t -> 'a path t
    | Text_file_lines : #text_file path t -> string list t

  let string s = Const s
  let list xs = List xs
  let input fn = Input fn

  let load x = Load_value x
  let _if_ cond _then_ _else_ = If { cond ; _then_ ; _else_ }
  let map x ~f = Map (x, f)

  module Command = struct
    include Command
    let dep x = dep (Any x)
  end

  let cmd ?stdout x args = Command Command.(cmd ?stdout x args)

  module Unix_utils = struct
    let grep pat x =
      cmd "grep" ~stdout:Command.dest Command.[
          quote ~using:'\'' (string pat) ;
          dep x
        ]

    let wget ?no_check_certificate ?user ?password (url : string t) =
      cmd "wget" Command.[
          option (flag string "--no-check-certificate") no_check_certificate ;
          option (opt "--user" string) user ;
          option (opt "--password" string) password ;
          opt "-O" ident dest ;
          dep url ;
        ]
  end

  module Text_file = struct
    let lines x = Text_file_lines x
  end
end

module Engine_lwt() = struct
  open Lwt
  include Engine

  type status = [`RUNNING | `DONE | `FAILED]

  let cache = String.Table.create ()

  let rec eval : type s. s t -> s Lwt.t = function
    | Const x -> Lwt.return x

    | List xs ->
      let children = List.map xs ~f:eval in
      Lwt_list.map_p ident children

    | Input fn ->
      Lwt.return (Path fn)

    | Load_value v ->
      eval v >>= fun (Path fn) ->
      Lwt_io.(with_file input fn read_value)

    | _ -> assert false

  let run expr =
    ()
end

module Run = NLP_wikipedia(Engine)
