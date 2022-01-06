type t = Color.t array array

let dimx = 8
let dimy = 8

let mindex_of_aindex ~dimx ~dimy index = index / dimx, index % dimy

let empty () = Array.make_matrix ~dimx ~dimy Color.Blank

let of_array array =
  let color_array = match array with
    | `Ints a -> Array.map a ~f:Color.of_array
    | `Colors c -> c
  in
  let matrix = empty () in
  Array.iteri color_array (fun i color ->
      let i, j = mindex_of_aindex ~dimx ~dimy i in
      matrix.(i).(j) <- color);
  matrix

let to_array = Array.concat_map ~f:(Array.map ~f:Color.to_array)

let array_of_pyarray =
  Py.List.(to_array_map (to_array_map Py.Int.to_int))

let pyarray_of_array =
  Py.List.(of_array_map (of_array_map Py.Int.of_int))

let of_pyarray pyarray = of_array (`Ints (array_of_pyarray pyarray))

let to_pyarray matrix = pyarray_of_array @@ to_array matrix
