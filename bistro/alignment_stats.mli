open Bistro
open Bistro_bioinfo

val bamstats : bam workflow -> Biocaml_unix.Bamstats.t sexp_value workflow

val fragment_length_stats : bam workflow -> (int * int) list sexp_value workflow

val chrstats : bam workflow -> (string * int) list sexp_value workflow

val summary :
  string list ->
  Biocaml_unix.Bamstats.t sexp_value workflow list ->
  (string * int) list sexp_value workflow list ->
  html workflow
