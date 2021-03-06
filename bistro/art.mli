(** Note: I experienced problems with this program, namely that often
    the produced fastq file has incorrect syntax (reads that are
    shorter than the quality read. *)

open Bistro
open Bistro_bioinfo

type _ tbool =
  | True  : [`True] tbool
  | False : [`False] tbool

type _ read_model =
  | Single_end : int -> [`single_end] read_model
  | Paired_end : paired_end -> [`paired_end] read_model

and paired_end = {
  len : int ; (** read length *)
  mflen : float ; (** mean fragment length *)
  sdev : float ; (** std variation on fragment length *)
  matepair : bool ;
}

type 'a art_illumina_output = 'a
  constraint 'a = < aln : _ ; errfree_sam : _ ; sam : _ ; read_model : _ >

val art_illumina :
  ?qprof1:string ->
  ?qprof2:string ->
  ?amplicon:bool ->
  ?id:string ->
  ?insRate:float ->
  ?insRate2:float ->
  ?delRate:float ->
  ?delRate2:float ->
  ?maskN:int ->
  ?qShift:float ->
  ?qShift2:float ->
  ?rndSeed:float ->
  ?sepProf:bool ->
  ?seqSys:[< `GA1 | `GA2 | `HS10 | `HS20 | `HS25 | `MS ] ->
  ?cigarM:bool ->
  aln_output:'a tbool ->
  errfree_sam_output:'b tbool ->
  sam_output:'c tbool ->
  'rm read_model ->
  [< `Coverage_fold of float | `Read_count of int ] ->
  fasta pworkflow ->
  < aln : 'a;
    errfree_sam : 'b;
    read_model : 'rm;
    sam : 'c > art_illumina_output dworkflow

val se_fastq :
  < read_model : [`single_end] ; .. > art_illumina_output dworkflow ->
  sanger_fastq pworkflow

val pe_fastq :
  [`One | `Two] ->
  < read_model : [`paired_end] ; .. > art_illumina_output dworkflow ->
  sanger_fastq pworkflow
