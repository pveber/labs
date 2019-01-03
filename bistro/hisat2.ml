open Core
open Bistro
open Bistro_bioinfo
open Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"hisat2" ~tag:"2.1.0" () ]

let hisat2_build ?large_index ?noauto ?packed ?bmax ?bmaxdivn ?dcv ?nodc ?noref ?justref ?offrate ?ftabchars ?seed ?cutoff fa =
  Workflow.shell ~descr:"hisat2-build" ~mem:(Workflow.int (8 * 1024)) ~np:8 [
    mkdir_p dest ;
    cmd "hisat2-build" ~img [
      option (flag string "--large-index") large_index ;
      option (flag string "--no-auto") noauto ;
      option (flag string "--packed") packed ;
      option (flag string "--nodc") nodc ;
      option (flag string "--noref") noref ;
      option (flag string "--justref") justref ;
      option (opt "--bmax" int) bmax ;
      option (opt "--bmaxdivn" int) bmaxdivn ;
      option (opt "--dcv" int) dcv ;
      option (opt "--offrate" int) offrate ;
      option (opt "--ftabchars" int) ftabchars ;
      opt "--threads" ident np ;
      option (opt "--seed" int) seed ;
      option (opt "--cutoff" int) cutoff ;
      opt "-f" dep fa ;
      seq [ dest ; string "/index" ]
    ]
  ]

let qual_option (type s) x = match (x : s Fastq.format) with
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq. Phred64 -> "--phred64-quals"

let flag_of_orientation = function
  | `fr -> "--fr"
  | `rf -> "--rf"
  | `ff -> "--ff"

let hisat2
    ?skip ?qupto ?trim5 ?trim3 ?fastq_format
    ?k
    ?minins ?maxins ?orientation ?no_mixed ?no_discordant
    ?seed
    index
    fqs
  =
  let args = match fqs with
    | `single_end fqs ->
      opt "-U" (list dep ~sep:",") fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        opt "-1" (list dep ~sep:",") fqs1 ;
        string " " ;
        opt "-2" (list dep ~sep:",") fqs2
      ]
  in
  Workflow.shell ~descr:"hisat2" ~mem:(Workflow.int (4 * 1024)) ~np:8 [
    cmd "hisat2" ~img [
      option (opt "--skip" int) skip ;
      option (opt "--qupto" int) qupto ;
      option (opt "--trim5" int) trim5 ;
      option (opt "--trim3" int) trim3 ;
      option (opt "-k" int) k ;
      option (opt "--minins" int) minins ;
      option (opt "--maxins" int) maxins ;
      option (flag_of_orientation % string) orientation ;
      option (flag string "--no-mixed") no_mixed  ;
      option (flag string "--no-discordant") no_discordant  ;
      opt "--threads" ident np ;
      option (opt "--seed" int) seed ;
      option (opt "-q" (qual_option % string)) fastq_format ;

      opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
      args ;
      opt "-S" ident dest ;
    ]
  ]
