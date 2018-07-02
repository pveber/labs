open Core_kernel
open Bistro
open Bistro_bioinfo

let%bistro[@version 2] bamstats (bam : bam workflow) =
  let open Biocaml_ez in
  let open CFStream in
  Bam.with_file [%dep bam] ~f:(fun _ als ->
      Stream.fold als ~init:Bamstats.zero ~f:Bamstats.update
    )
  |> Bamstats.sexp_of_t
  |> Sexp.to_string_hum
  |> fun data -> Out_channel.write_all [%dest] ~data

let%bistro fragment_length_stats (bam : bam workflow) =
  let open Biocaml_ez in
  let open CFStream in
  Bam.with_file0 [%dep bam] ~f:Bamstats.Fragment_length_histogram.(fun _ als ->
      let h = create ~min_mapq:5 () in
      Stream.iter als ~f:(fun al -> ok_exn (update0 h al)) ;
      Biocaml_unix.Accu.Counter.to_alist h.counts
    ) ;
  |> [%sexp_of: (int * int) list]
  |> Sexp.to_string_hum
  |> fun data -> Out_channel.write_all [%dest] ~data

let%bistro chrstats (bam : bam workflow) =
  let open Biocaml_ez in
  let open CFStream in
  Bam.with_file0 [%dep bam] ~f:Bamstats.Chr_histogram.(fun header als ->
      let h = create ~min_mapq:5 header in
      Stream.iter als ~f:(fun al -> ok_exn (update0 h al)) ;
      Biocaml_unix.Accu.Counter.to_alist h.counts
    ) ;
  |> [%sexp_of: (string * int) list]
  |> Sexp.to_string_hum
  |> fun data -> Out_channel.write_all [%dest] ~data

let%bistro summary samples bamstats chrstats =
  let open Biocaml_ez in
  let open Tyxml_html in
  let k = pcdata in
  let stats = List.map [%deps bamstats] ~f:(fun fn ->
      In_channel.read_all fn
      |> Sexp.of_string
      |> Bamstats.t_of_sexp
    )
  in
  let chrstats = match [%deps chrstats] with
    | [] -> None
    | xs ->
      Some (
        List.map xs ~f:(fun fn ->
            In_channel.read_all fn
            |> Sexp.of_string
            |> [%of_sexp: (string * int) list]
          )
      )
  in
  let chrstat_table samples = function
    | [] -> raise (Invalid_argument "chrstat_table: needs at least one sample")
    | stats ->
      let all_chr =
        List.map stats ~f:(List.map ~f:fst)
        |> List.concat
        |> List.dedup_and_sort ~compare:String.compare
      in
      let header =
        thead [
          tr (
            th [k "Sample"] :: List.map all_chr ~f:(fun chr -> th [k chr])
          ) ;
        ]
      in
      let line sample stats =
        let n = List.fold stats ~init:0 ~f:(fun acc (_, n) -> acc + n) in
        let cols = List.map stats ~f:(fun (_, k) ->
            let p = Float.(of_int k / of_int n *. 100.) in
            td [ pcdata (sprintf "%.1f%%" p) ] ;
          )
        in
        tr (td [ k sample ] :: cols)
      in
      let lines = List.map2_exn samples stats ~f:line in
      table ~thead:header ~a:[a_class ["table"]] lines
  in
  let flagstat_table samples stats =
    let fraction n k =
      let f =
        if n <= 0 then 0.
        else Float.(of_int k / of_int n * 100.)
      in
      pcdata (sprintf "%d (%.1f%%)" k f)
    in
    let header =
      thead [
        tr [
          th [k "Sample"] ;
          th [k "Total"] ;
          th [k "QC pass"] ;
          th [k "Mapped reads"] ;
          th [k "Read pairs"] ;
          th [k "Mapped pairs"] ;
        ]
      ]
    in
    let line sample { Bamstats.total ;
                      qc_pass ;
                      read_pairs ;
                      mapped_reads ;
                      mapped_pairs ; _ } =
      tr [
        td [ k sample ] ;
        td [ k Int.(to_string total) ] ;
        td [ fraction total qc_pass ] ;
        td [ fraction total mapped_reads ] ;
        td [ k Int.(to_string read_pairs) ] ;
        td [ fraction read_pairs mapped_pairs ] ;
      ]
    in
    let lines = List.map2_exn samples stats ~f:line in
    table ~thead:header ~a:[a_class ["table"]] lines
  in
  let contents = [
    flagstat_table samples stats ;
    br () ;
    Option.value_map chrstats ~default:(pcdata "") ~f:(chrstat_table samples) ;
  ]
  in
  Labs_croquis.Html_page.(
    to_file
      (make ~title:"Alignment summary" contents)
      [%dest]
  )
