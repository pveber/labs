open Core
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"kallisto" ~tag:"0.43.0" ()

type kallisto_output

let index fas =
  workflow ~descr:"kallisto-index" [
    cmd "kallisto index" ~env [
      opt "-i" ident dest ;
      list ~sep:" " dep fas ;
    ]
  ]

(* process substitution for gunzip *)
let psgunzip x =
  seq ~sep:"" [ string "<(gunzip -c " ; dep x ; string ";)" ]

let fq_input = function
  | `fq_gz x -> psgunzip x
  | `fq x -> dep x

let quant ?bootstrap_samples idx fq1 fq2 : kallisto_output directory workflow =
  workflow ~descr:"kallisto-quant" ~np:4 [
    cmd "kallisto quant" ~env [
      opt "-i" dep idx ;
      opt "-o" ident dest ;
      opt "-t" ident np ;
      option (opt "-b" int) bootstrap_samples ;
      fq_input fq1 ;
      fq_input fq2 ;
    ]
  ]

let abundance : (kallisto_output, [`tsv]) selector =
  selector [ "abundance.tsv" ]


let%bistro merge_eff_counts ~sample_ids ~kallisto_outputs =

  let parse_eff_counts fn =
    In_channel.read_lines fn
    |> Fn.flip List.drop 1
    |> List.map ~f:(fun l ->
        String.split ~on:'\t' l
        |> Fn.flip List.nth_exn 3
      )
  in
  let parse_names fn =
    In_channel.read_lines fn
    |> Fn.flip List.drop 1
    |> List.map ~f:(fun l ->
        String.split ~on:'\t' l
        |> List.hd_exn
      )
  in

  let names = parse_names (List.hd_exn [%deps kallisto_outputs]) in
  let counts  = List.map [%deps kallisto_outputs] ~f:parse_eff_counts in

  let table = List.transpose_exn (names :: counts) in

  let lines =
    ("transcript" :: sample_ids) :: table
    |> List.map ~f:(String.concat ~sep:"\t")
  in

  Out_channel.write_lines [%dest] lines


let%bistro merge_tpms ~sample_ids ~kallisto_outputs =

  let parse_tpms fn =
    In_channel.read_lines fn
    |> Fn.flip List.drop 1
    |> List.map ~f:(fun l ->
        String.split ~on:'\t' l
        |> Fn.flip List.nth_exn 4
      )
  in
  let parse_names fn =
    In_channel.read_lines fn
    |> Fn.flip List.drop 1
    |> List.map ~f:(fun l ->
        String.split ~on:'\t' l
        |> List.hd_exn
      )
  in

  let names = parse_names (List.hd_exn [%deps kallisto_outputs]) in
  let tpms  = List.map [%deps kallisto_outputs] ~f:parse_tpms in

  let table = List.transpose_exn (names :: tpms) in

  let lines =
    ("transcript" :: sample_ids) :: table
    |> List.map ~f:(String.concat ~sep:"\t")
  in

  Out_channel.write_lines [%dest] lines
