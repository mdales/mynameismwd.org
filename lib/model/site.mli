type t

val of_directory : Fpath.t -> t
val sections : t -> Section.t list
val title : t -> string
val toplevel : t -> Section.t
