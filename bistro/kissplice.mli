open Bistro
open Bistro_bioinfo

val img : Shell_dsl.container_image list

val kissplice :
  ?max_memory:[`GB of int] ->
  k:int ->
  sanger_fastq pworkflow ->
  sanger_fastq pworkflow ->
  [`kissplice] dworkflow
