type t = char array * (char * Color.t) list

val of_file : string -> t

val to_matrix : t -> Matrix.t
