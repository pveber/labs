open Bistro.Std
open Bistro_bioinfo.Std

val env : docker_image

val kissplice :
  ?max_memory:[`GB of int] ->
  k:int ->
  'a fastq workflow ->
  'a fastq workflow ->
  [`kissplice] directory workflow
