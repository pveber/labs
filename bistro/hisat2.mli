open Bistro
open Bistro_bioinfo

val env : Shell_dsl.docker_image

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
  fasta pworkflow ->
  [`hisat2_index] dworkflow


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
  [`hisat2_index] dworkflow ->
  [ `single_end of sanger_fastq pworkflow list
  | `paired_end of sanger_fastq pworkflow list * sanger_fastq pworkflow list ] ->
  sam pworkflow
