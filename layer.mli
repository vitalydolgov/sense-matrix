type t = char array * (char * Color.t) list

val from_file : string -> t

val to_matrix : t -> Matrix.t
