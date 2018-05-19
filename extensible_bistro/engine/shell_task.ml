type file_dump = File_dump of {
    text : string ;
    path : string ;
  }

type outcome = {
  status : [`Succeeded | `Missing_output | `Failed] ;
  exit_code : int ;
  cmd : string ;
  file_dumps : file_dump list ;
  cache : string option ;
  stdout : string ;
  stderr : string ;
}
