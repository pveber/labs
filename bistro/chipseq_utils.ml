let%bistro gff_of_bed bed =
  let open Biocaml_base in
  let open Labs_biopipes.Biopipes in
  run (
    from_file [%dep bed]
    $$ bed_parser ()
    $$ map (function
        | (chr, start_pos, stop_pos, name :: _) ->
          `Record (
            Gff.record
              ~strand:`Not_stranded
              ~attributes:["gene_id", [name]]
              ~feature:"peak"
              chr start_pos stop_pos
          )
        | _ -> failwith "gff_of_bed: expect at least bed4"
      )
    $$ gff_unparser `two
    $$ to_file [%dest]
  )

