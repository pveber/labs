open Bistro
open Bistro_bioinfo

val env : Shell_dsl.docker_image

val kissplice :
  ?max_memory:[`GB of int] ->
  k:int ->
  sanger_fastq pworkflow ->
  sanger_fastq pworkflow ->
  [`kissplice] dworkflow
