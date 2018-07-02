open Bistro
open Bistro_bioinfo

val env : docker_image

val kissplice :
  ?max_memory:[`GB of int] ->
  k:int ->
  'a fastq workflow ->
  'a fastq workflow ->
  [`kissplice] directory workflow
