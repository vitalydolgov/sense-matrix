type t = Color.t array array

val empty : unit -> t

val of_array : [< `Ints of int array array | `Colors of Color.t array ] -> t
val to_array : t -> int array array

val of_pyarray : Pytypes.pyobject -> t
val to_pyarray : t -> Pytypes.pyobject

val dimx : int
val dimy : int
