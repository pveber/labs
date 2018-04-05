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

  val input : string -> #path t
  val load : 'a ocaml_value t -> 'a t
  val map : 'a list t -> ('a t -> 'b t) -> 'b list t
  val _if_ : bool t -> 'a t -> 'a t

  module Unix_utils : sig
    val wget : string -> 'a t
    val grep : string -> #text_file t -> #text_file t
  end

  module Text_file : sig
    val first_line : #text_file t -> string t
    val lines : #text_file t -> string list t
  end
end


module NLP_wikipedia(L : Lang) = struct
  open L

  let protein_wikipedia_page =
    Unix_utils.wget "https://en.wikipedia.org/wiki/Protein"

  let links =
    Unix_utils.grep "https://en.wikipedia.org/wiki/[^ ]+" protein_wikipedia_page
    |> Text_file.lines

  let pages =
    map links Unix_utils.wget
end

module Eval = struct
end
