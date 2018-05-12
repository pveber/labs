class type ['a] directory_format = object
  method path_kind : [`Directory]
  method contents : 'a
end

class type file_format = object
  method path_kind : [`File]
end

class type binary_encoded = object
  inherit file_format
  method encoding : [`Binary_encoding]
end

class type text_encoded = object
  inherit file_format
  method encoding : [`Text_encoding]
end

class type html = object
  inherit text_encoded
  method format : [`Html]
end

(** Conventional type to represent OCaml values saved with the
    {!module:Marshal} module. *)
class type ['a] marshalled_value = object
  inherit binary_encoded
  method format : [`marshalled_value]
  method content_type : 'a
end

class type ['a] zip = object
  inherit binary_encoded
  method format : [`zip]
  method content_format : 'a
end

class type ['a] gz = object
  constraint 'a = #file_format
  inherit binary_encoded
  method format : [`gz]
  method content_format : 'a
end

class type ['a] bz2 = object
  constraint 'a = #file_format
  inherit binary_encoded
  method format : [`bz2]
  method content_format : 'a
end

class type ['a] tar = object
  inherit binary_encoded
  method format : [`tar]
  method content_format : 'a
end

class type text_file = object
  inherit file_format
  method encoding : [`text]
end

(** Conventional type to represent OCaml values saved as
    S-expressions. *)
class type ['a] sexp_value = object
  inherit text_file
  method format : [`sexp_value]
  method content_type : 'a
end

class type pdf = object
  inherit text_file
  method format : [`pdf]
end

class type png = object
  inherit binary_encoded
  method format : [`png]
end

class type svg = object
  inherit text_file
  method format : [`svg]
end

class type tsv = object
  inherit text_file
  method colum_separator : [`tab]
end
