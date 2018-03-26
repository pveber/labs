open Bistro.EDSL
open Bistro_bioinfo

let docker_image =
  docker_image ~account:"pveber" ~name:"picard-tools" ~tag:"2.8.1" ()

let markduplicates ?remove_duplicates indexed_bam =
  let arg k v =
    seq ~sep:"" [ string k ; string "=" ; v ]
  in
  workflow ~descr:"picard.markduplicates" ~mem:(3 * 1024) [
    mkdir_p dest ;
    cmd "PicardCommandLine" ~env:docker_image [
      string "MarkDuplicates" ;
      arg "INPUT" (dep @@ indexed_bam / Samtools.indexed_bam_to_bam) ;
      arg "OUTPUT" (dest // "reads.bam") ;
      arg "METRICS_FILE" (dest // "dup_qc") ;
      string "VALIDATION_STRINGENCY=LENIENT" ;
      string "ASSUME_SORT_ORDER=coordinate" ;
      option (Printf.sprintf "REMOVE_DUPLICATES=%b" % string) remove_duplicates ;
    ]
  ]

let reads = selector ["reads.bam"]
