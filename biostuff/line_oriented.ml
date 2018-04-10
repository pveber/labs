open Core
open Biocaml_base

let lines () =
  let open Lines.Parser in
  Pipes_unix.Pipe.loop step initial_state

module type Item = sig
  type item
  val parse : Line.t -> item
end

module type S = sig
  type item
  val load : string -> item list
  val fold : string -> init:'a -> f:('a -> item -> 'a) -> 'a
end

module Make(Item : Item) = struct
  let load fn =
    In_channel.read_lines fn
    |> List.map ~f:(fun l ->
        Item.parse (Line.of_string_unsafe l)
      )

  let fold fn ~init ~f =
    let open Pipes_unix.Pipe in
    run (
      from_file fn
      $$ lines ()
      $$ map Item.parse
      $$ fold init (Fn.flip f)
    )

end
