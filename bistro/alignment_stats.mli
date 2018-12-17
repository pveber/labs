open Bistro
open Bistro_bioinfo

val bamstats : bam pworkflow -> Biocaml_unix.Bamstats.t sexp_value pworkflow

val fragment_length_stats : bam pworkflow -> (int * int) list sexp_value pworkflow

val chrstats : bam pworkflow -> (string * int) list sexp_value pworkflow

val summary :
  string list ->
  Biocaml_unix.Bamstats.t sexp_value pworkflow list ->
  (string * int) list sexp_value pworkflow list ->
  html pworkflow
