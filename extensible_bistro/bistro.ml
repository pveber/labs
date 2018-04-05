open Core_kernel

class type path = object
  method path : string
end

class type file = object
  inherit path
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

class type ['a] ocaml_value = object
  inherit binary_file
  method load : 'a
end

type bbool = bool ocaml_value
type bstring = string ocaml_value
type 'a blist = 'a list ocaml_value

module type Lang = sig
  type 'a t

  val string : string -> string t
  val list : 'a t list -> 'a list t
  val input : string -> #path t
  val load : 'a ocaml_value t -> 'a t
  val map : 'a list t -> f:('a t -> 'b t) -> 'b list t
  val _if_ : bool t -> 'a t -> 'a t -> 'a t

  module Unix_utils : sig
    val wget :
      ?no_check_certificate:bool ->
      ?user:string ->
      ?password:string ->
      string t -> #path t
    val grep : string -> #text_file t -> #text_file t
  end

  module Text_file : sig
    val lines : #text_file t -> string list t
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

  let pages =
    map links ~f:Unix_utils.wget
end

module Engine = struct
  type 'a t = ..

  type _ t +=
    | Const : 'a -> 'a t
    | List  : 'a t list -> 'a list t
    | Input_file : string -> #path t
    | Load_value : 'a ocaml_value t -> 'a t
    | If : { cond : bool t ; _then_ : 'a t ; _else_ : 'a t } -> 'a t
    | Map : 'a list t * ('a t -> 'b t) -> 'b list t
    | Command : 'a t Command.t -> #path t
    | Text_file_lines : #text_file t -> string list t

  let string s = Const s
  let list xs = List xs
  let input fn = Input_file fn

  let load x = Load_value x
  let _if_ cond _then_ _else_ = If { cond ; _then_ ; _else_ }
  let map x ~f = Map (x, f)

  let cmd ?stdout x args = Command Command.(cmd ?stdout x args)

  module Unix_utils = struct
    let grep pat x =
      cmd "grep" ~stdout:Command.dest Command.[
          quote ~using:'\'' (string pat) ;
          dep x
        ]

    let wget ?no_check_certificate ?user ?password url =
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

module Run = NLP_wikipedia(Engine)
