open Bistro.Std
open Bistro_bioinfo.Std

val env : docker_image

type index = [`hisat2_index] directory

val hisat2_build :
  ?large_index:bool ->
  ?noauto:bool ->
  ?packed:bool ->
  ?bmax:int ->
  ?bmaxdivn:int ->
  ?dcv:int ->
  ?nodc:bool ->
  ?noref:bool ->
  ?justref:bool ->
  ?offrate:int ->
  ?ftabchars:int ->
  ?seed:int ->
  ?cutoff:int ->
  fasta workflow ->
  index workflow


val hisat2 :
  ?skip:int ->
  ?qupto:int ->
  ?trim5:int ->
  ?trim3:int ->
  ?fastq_format:'a Fastq.format ->
  ?k:int ->
  ?minins:int ->
  ?maxins:int ->
  ?orientation:[`fr | `ff | `rf] ->
  ?no_mixed:bool ->
  ?no_discordant:bool ->
  ?seed:int ->
  index workflow ->
  [ `single_end of 'a fastq workflow list
  | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
  sam workflow
