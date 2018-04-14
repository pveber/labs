open Biocaml_base

module type Item = sig
  type t
  val parse : Line.t -> t
end

module type S = sig
  type item
  val load : string -> item list
  val fold : string -> init:'a -> f:('a -> item -> 'a) -> 'a
end

module Make(Item : Item) : S with type item = Item.t
