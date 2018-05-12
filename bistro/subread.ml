open Core
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"subread" ~tag:"1.6.0" ()

class type count_table = object
  inherit tsv
  method header : [`none]
  method f1 : string
  method f2 : int
end

let strandness_token = function
  | `Unstranded -> int 0
  | `Stranded -> int 1
  | `Reversely_stranded -> int 2

let featureCounts
    ?feature_type ?attribute_type ?strandness
    ?q ?nthreads
    gff mapped_reads =
  workflow ~descr:"featureCounts" ~np:(Option.value ~default:1 nthreads) [
    mkdir_p dest ;
    cmd "featureCounts" ~env [
      option (opt "-t" string) feature_type ;
      option (opt "-g" string) attribute_type ;
      option (opt "-s" strandness_token) strandness ;
      option (opt "-Q" int) q ;
      option (opt "-T" (fun _ -> np)) nthreads ;
      opt "-a" dep gff ;
      opt "-o" ident (dest // "counts.tsv") ;
      dep mapped_reads ;
    ]
  ]

let featureCounts_tsv = selector ["counts.tsv"]
let featureCounts_summary = selector ["counts.tsv.summary"]
