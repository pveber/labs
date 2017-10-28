open Bistro.EDSL

type 'a format = NarrowPeak | BroadPeak | Bed | Gff

let narrowPeak = NarrowPeak
let broadPeak = BroadPeak
let bed = Bed
let gff = Gff

type output = [`idr_output]

let string_of_file_format = function
  | NarrowPeak -> "narrowPeak"
  | BroadPeak -> "broadPeak"
  | Bed -> "bed"
  | Gff -> "gff"

let file_format x = string (string_of_file_format x)

let string_of_merge_method = function
  | `sum -> "sum"
  | `avg -> "avg"
  | `min -> "min"
  | `max -> "max"

let merge_method x = string (string_of_merge_method x)

let env = docker_image ~account:"pveber" ~name:"idr" ~tag:"2.0.3" ()

let idr
    ~input_file_type ?idr_threshold ?soft_idr_threshold
    ?peak_merge_method ?random_seed
    samples =
  workflow ~descr:"Idr.idr" [
    mkdir_p dest ;
    cmd "idr" ~env [
      opt "--input-file-type" file_format input_file_type ;
      opt "--output-file" (fun x -> x) (dest // "items.tsv") ;
      option (opt "--idr-threshold" float) idr_threshold ;
      option (opt "--soft-idr-threshold" float) soft_idr_threshold ;
      option (opt "--peak-merge-method" merge_method) peak_merge_method ;
      string "--plot" ;
      opt "--samples" (list ~sep:" " dep) samples ;
    ]
  ]

let items = selector [ "items.tsv" ]
let figure = selector [ "items.tsv.png" ]