open Core
open Bistro
open Shell_dsl

type index = [`star_index] directory

let env = docker_image ~account:"flemoine" ~name:"star" ()

let mem_in_bytes = seq ~sep:" " [string "$((" ; mem ; string " * 1024 * 1024))$"]

let genomeGenerate fa =
  shell ~descr:"star.index" ~np:8 ~mem:(30 * 1024) [
    mkdir_p dest ;
    cmd "STAR" ~env [
      opt "--runThreadN" ident np ;
      opt "--runMode" string "genomeGenerate" ;
      opt "--genomeDir" ident dest ;
      opt "--genomeFastaFiles" dep fa ;
      opt "--limitGenomeGenerateRAM" ident mem_in_bytes ;
    ]
  ]

let fq_args = function
  | `paired_end (fq1, fq2) ->
    [dep fq1 ; dep fq2]
  | `single_end fq -> [ dep fq ]

let samStrandField = function
  | `None -> string "None"
  | `intronMotif -> string "intronMotif"

let alignReads ?(max_mem = `GB 8)
    ?outFilterMismatchNmax
    ?outFilterMultimapNmax
    ?outSAMstrandField
    ?alignIntronMax idx fqs =
  let `GB max_mem = max_mem in
  shell ~descr:"star.map" ~np:8 ~mem:(max_mem * 1024) [
    mkdir_p dest ;
    cmd "STAR" ~stdout:(dest // "sorted.bam") ~env [
      opt "--outFileNamePrefix" ident (dest // "star") ;
      opt "--runThreadN" ident np ;
      option (opt "--outSAMstrandField" samStrandField) outSAMstrandField ;
      option (opt "--outFilterMismatchNmax" int) outFilterMismatchNmax ;
      option (opt "--outFilterMultimapNmax" int) outFilterMultimapNmax ;
      opt "--genomeDir" dep idx ;
      opt "--readFilesIn" ident (seq ~sep:" " (fq_args fqs)) ;
      opt "--outSAMunmapped" string "None" ;
      opt "--outStd" string "SAM" ;
      opt "--genomeLoad" string "NoSharedMemory" ;
      (* opt "--limitBAMsortRAM" ident mem_in_bytes ; *)
    ]
  ]

let sorted_mapped_reads = selector ["sorted.bam"]
