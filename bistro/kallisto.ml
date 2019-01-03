open Core
open Bistro
open Shell_dsl

class type index = object
  inherit binary_file
  method format : [`kallisto_index]
end

class type abundance_table = object
  inherit tsv
  method f1 : [`target_id] * string
  method f2 : [`length] * int
  method f3 : [`eff_length] * int
  method f4 : [`est_counts] * float
  method f5 : [`tpm] * float
end

let img = [ docker_image ~account:"pveber" ~name:"kallisto" ~tag:"0.43.0" () ]

let index fas =
  Workflow.shell ~descr:"kallisto-index" [
    cmd "kallisto index" ~img [
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

let quant ?bootstrap_samples ?threads ?fragment_length ?sd idx ~fq1 ?fq2 () =
  Workflow.shell ~descr:"kallisto-quant" ?np:threads [
    cmd "kallisto quant" ~img [
      opt "-i" dep idx ;
      opt "-o" ident dest ;
      opt "-t" ident np ;
      option (opt "-b" int) bootstrap_samples ;
      fq_input fq1 ;
      option fq_input fq2 ;
      option (opt "-l" float) fragment_length ;
      option (opt "-s" float) sd ;
      string (
        match fq2 with
        | None -> "--single"
        | Some _ -> ""
      ) ;
    ]
  ]

let abundance x =
  Workflow.select x [ "abundance.tsv" ]


let%pworkflow merge_eff_counts ~sample_ids ~kallisto_outputs =

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

  let names = parse_names [%eval Workflow.eval_path (List.hd_exn kallisto_outputs)] in
  let counts  = List.map [%eval Workflow.(spawn (list kallisto_outputs) ~f:eval_path)] ~f:parse_eff_counts in

  let table = List.transpose_exn (names :: counts) in

  let lines =
    ("transcript" :: sample_ids) :: table
    |> List.map ~f:(String.concat ~sep:"\t")
  in

  Out_channel.write_lines [%dest] lines


let%pworkflow merge_tpms ~sample_ids ~kallisto_outputs =

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

  let names = parse_names [%eval Workflow.eval_path (List.hd_exn kallisto_outputs)] in
  let tpms  = List.map [%eval Workflow.(spawn (list kallisto_outputs) ~f:eval_path)] ~f:parse_tpms in

  let table = List.transpose_exn (names :: tpms) in

  let lines =
    ("transcript" :: sample_ids) :: table
    |> List.map ~f:(String.concat ~sep:"\t")
  in

  Out_channel.write_lines [%dest] lines
