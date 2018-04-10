open Core_kernel
open CFStream
open Stream.Infix

module Map = struct
  include Map.Make(String)

  let to_stream t = Stream.of_list (to_alist t)
  let of_stream xs =
    Stream.fold xs ~init:empty ~f:(fun accu (key,data) -> set accu ~key ~data)
end

module Accu = Biocaml_unix.Accu

module Selection = struct
  module Iset = Biocaml_unix.Iset
  type t = Iset.t Map.t

  let empty = Map.empty
  let add sel GLoc.{ chr ; st ; ed } =
    let set_chr =
      match Map.find sel chr with
      | None -> Iset.empty
      | Some s -> s
    in
    let set_chr = Iset.add_range set_chr st ed in
    Map.set sel ~key:chr ~data:set_chr

  let inter u v =
    Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
        match Map.find v k with
        | Some set_v -> Map.set accu ~key:k ~data:(Iset.inter set_u set_v)
        | None -> accu
      )

  let union u v =
    let keys = List.dedup_and_sort ~compare:String.compare (Map.keys u @ Map.keys v) in
    List.fold keys ~init:Map.empty ~f:(fun accu k ->
        Map.set accu ~key:k ~data:(
          Iset.union
            (Option.value (Map.find u k) ~default:Iset.empty)
            (Option.value (Map.find v k) ~default:Iset.empty)
        )
      )

  let diff u v =
    Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
        let set_u' =
          match Map.find v k with
          | Some set_v -> Iset.diff set_u set_v
          | None -> set_u
        in
        Map.set ~key:k ~data:set_u' accu
      )

  let size x =
    Map.fold x ~init:0 ~f:(fun ~key:_ ~data:set accu -> Iset.cardinal set + accu)

  let overlap sel GLoc.{ chr ; st ; ed } = Iset.(
      match Map.find sel chr with
      | Some x ->
        inter (add_range empty st ed) x
        |> cardinal
      | None -> 0
    )

  let intersects sel GLoc.{ chr ; st ; ed } =
    Option.value_map
      (Map.find sel chr)
      ~default:false
      ~f:(fun x -> Iset.intersects_range x st ed)

  let to_stream sel =
    Map.to_stream sel
    |> Stream.map ~f:(fun (chr, s) ->
        Stream.map (Iset.to_stream s) ~f:(fun (st, ed) ->
            GLoc.{ chr ; st ; ed }
          )
      )
    |> Stream.concat

  let of_stream e =
    let accu =
      Accu.create
        ~bin:(fun x -> x.GLoc.chr)
        ~zero:Iset.empty
        ~add:(fun GLoc.{ st ; ed } x -> Iset.add_range x st ed)
        ()
    in
    Stream.iter ~f:(fun loc -> Accu.add accu loc loc) e ;
    Map.of_stream (Accu.stream accu)
end

module LMap = struct
  module T = Biocaml_unix.Interval_tree

  type 'a t = 'a T.t Map.t

  let intersects lmap { GLoc.chr ; st ; ed }  =
    Option.value_map
      (Map.find lmap chr)
      ~default:false
      ~f:(fun x -> T.intersects x ~low:st ~high:ed)

  let closest lmap { GLoc.chr ; st ; ed } =
    Option.bind
      (Map.find lmap chr)
      ~f:(fun x ->
          try
            let st, ed, label, d = T.find_closest st ed x in
            Some ({ GLoc.chr ; st ; ed }, label, d)
          with T.Empty_tree -> None
        )

  let intersecting_elems lmap { GLoc.chr ; st ; ed } =
    match Map.find lmap chr with
    | Some x ->
      T.find_intersecting_elem st ed x
      /@ (fun (st, ed, x) -> { GLoc.chr ; st ; ed }, x)
    | None -> Stream.empty ()

  let to_stream lmap =
    (Map.to_stream lmap)
    /@ (fun (chr, t) ->
        Stream.map
          ~f:(fun (st, ed, x) -> { GLoc.chr ; st ; ed }, x)
          (T.to_stream t))
    |> Stream.concat

  let of_stream e =
    let accu =
      Accu.create
        ~bin:(fun { GLoc.chr } -> chr)
        ~zero:T.empty
        ~add:(fun ({ GLoc.st ; ed }, v) -> T.add ~data:v ~low:st ~high:ed)
        ()
    in
    Stream.iter ~f:(fun (loc, value) -> Accu.add accu loc (loc, value)) e ;
    Map.of_stream (Accu.stream accu)

end

module LSet = struct
  module T = Biocaml_unix.Interval_tree

  type t = unit T.t Map.t

  let intersects = LMap.intersects

  let closest lset loc =
    Option.map (LMap.closest lset loc) ~f:(fun (loc', (), d) -> loc', d)

  let intersecting_elems lset loc =
    LMap.intersecting_elems lset loc /@ fst

  let to_stream lset = LMap.to_stream lset /@ fst
  let of_stream e = e /@ (fun x -> x, ()) |> LMap.of_stream

end

