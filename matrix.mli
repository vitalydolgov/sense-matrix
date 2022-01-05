type t = Color.t array array

val empty : unit -> t

val of_array : int array array -> t
val to_array : t -> int array array

val of_pyarray : Pytypes.pyobject -> t
val to_pyarray : t -> Pytypes.pyobject
