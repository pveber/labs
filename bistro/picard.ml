open Bistro
open Bistro_bioinfo
open Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"picard-tools" ~tag:"2.8.1" () ]

let markduplicates ?remove_duplicates indexed_bam =
  let arg k v =
    seq ~sep:"" [ string k ; string "=" ; v ]
  in
  Workflow.shell ~descr:"picard.markduplicates" ~mem:(Workflow.int (3 * 1024)) [
    mkdir_p dest ;
    cmd "PicardCommandLine" ~img [
      string "MarkDuplicates" ;
      arg "INPUT" (dep @@ Samtools.indexed_bam_to_bam indexed_bam) ;
      arg "OUTPUT" (dest // "reads.bam") ;
      arg "METRICS_FILE" (dest // "dup_qc") ;
      string "VALIDATION_STRINGENCY=LENIENT" ;
      string "ASSUME_SORT_ORDER=coordinate" ;
      option (Printf.sprintf "REMOVE_DUPLICATES=%b" % string) remove_duplicates ;
    ]
  ]

let reads x = Workflow.select x ["reads.bam"]
