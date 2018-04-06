open Core_kernel

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

type 'a path = Path of string

class type file = object
  method kind : [`File]
end

class type binary_encoded = object
  inherit file
  method encoding : [`Binary_encoding]
end

class type text_encoded = object
  inherit file
  method encoding : [`Text_encoding]
end

class type html = object
  inherit text_encoded
  method format : [`Html]
end

class type ['a] ocaml_value = object
  inherit binary_encoded
  method load : 'a
end

type bbool = bool ocaml_value
type bstring = string ocaml_value
type 'a blist = 'a list ocaml_value

module type Lang = sig
  type 'a t
  type 'a file = (#file as 'a) path t

  val string : string -> string t
  val list : 'a t list -> 'a list t
  val input : string -> _ path t
  val load : 'a ocaml_value file -> 'a t
  val map : 'a list t -> f:('a t -> 'b t) -> 'b list t
  val _if_ : bool t -> 'a t -> 'a t -> 'a t

  module Unix_utils : sig
    val wget :
      ?no_check_certificate:bool ->
      ?user:string ->
      ?password:string ->
      string t -> _ path t
    val grep : string -> #text_encoded file -> #text_encoded file
  end

  module Text_file : sig
    val lines : #text_encoded file -> string list t
  end
end


module NLP_wikipedia(L : Lang) = struct
  open L

  let protein_wikipedia_page : html file =
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

  type 'a file = (#file as 'a) path t

  type dep =
    | Path_dep : _ path t -> dep
    | String_dep : string t -> dep
    | Int_dep : int t -> dep

  type _ t +=
    | Const : 'a -> 'a t
    | List  : 'a t list -> 'a list t
    | Input : string -> 'a path t
    | Load_value : 'a ocaml_value path t -> 'a t
    | If : { cond : bool t ; _then_ : 'a t ; _else_ : 'a t } -> 'a t
    | Map : 'a list t * ('a t -> 'b t) -> 'b list t
    | Command : dep Command.t -> 'a path t
    | Text_file_lines : #text_encoded file -> string list t

  let string s = Const s
  let list xs = List xs
  let input fn = Input fn

  let load x = Load_value x
  let _if_ cond _then_ _else_ = If { cond ; _then_ ; _else_ }
  let map x ~f = Map (x, f)

  module Command = struct
    include Command
    let pdep x = dep (Path_dep x)
    let sdep x = dep (String_dep x)
  end

  let cmd ?stdout x args = Command Command.(cmd ?stdout x args)

  module Unix_utils = struct
    let grep pat x =
      cmd "grep" ~stdout:Command.dest Command.[
          quote ~using:'\'' (string pat) ;
          pdep x
        ]

    let wget ?no_check_certificate ?user ?password (url : string t) =
      cmd "wget" Command.[
          option (flag string "--no-check-certificate") no_check_certificate ;
          option (opt "--user" string) user ;
          option (opt "--password" string) password ;
          opt "-O" ident dest ;
          sdep url ;
        ]
  end

  module Text_file = struct
    let lines x = Text_file_lines x
  end
end

module Engine_lwt() = struct
  open Lwt
  include Engine

  module Command_map = struct
    let rec map_p x ~f =
      let open Command in
      match x with
      | Docker (img, cmd) ->
        map_p cmd ~f >|= fun cmd ->
        Docker (img, cmd)
      | Simple_command tmpl ->
        map_p_tokens tmpl ~f >|= fun tmpl ->
        Simple_command tmpl
      | And_list xs ->
        map_p_cmd_list xs ~f >|= fun xs -> And_list xs
      | Or_list xs ->
        map_p_cmd_list xs ~f >|= fun xs -> Or_list xs
      | Pipe_list xs ->
        map_p_cmd_list xs ~f >|= fun xs -> Pipe_list xs

    and map_p_cmd_list xs ~f = Lwt_list.map_p (map_p ~f) xs

    and map_p_tokens xs ~f =
      Lwt_list.map_p (map_p_token ~f) xs

    and map_p_token x ~f =
      let open Template in
      match x with
      | D x -> f x >|= fun y -> D y
      | DEST
      | NP
      | TMP
      | EXE
      | S _
      | MEM as x -> Lwt.return x
      | F tmpl ->
        map_p_tokens tmpl ~f >|= fun tmpl ->
        F tmpl
  end
  type status = [`RUNNING | `DONE | `FAILED]

  (* let cache = String.Table.create () *)

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

    | If { cond ; _then_ ; _else_ } ->
      eval cond >>= fun cond ->
      if cond then eval _then_ else eval _else_

    | Map (v, f) ->
      eval v >>= fun xs ->
      Lwt_list.map_p (fun x -> eval (f (Const x))) xs

    | Command cmd ->
      let f = function
        | Path_dep x ->
          eval x >|= fun (Path p) -> `Path p
        | String_dep x ->
          eval x >|= fun s -> `String s
        | Int_dep x ->
          eval x >|= fun i -> `Int i
      in
      Command_map.map_p cmd ~f >>= fun cmd ->
      Lwt.return (Path "")

    | Text_file_lines x ->
      eval x >>= fun (Path fn) ->
      Lwt_io.lines_of_file fn
      |> Lwt_stream.to_list

    | _ -> assert false

  let run expr =
    ()
end

module Run = NLP_wikipedia(Engine)
