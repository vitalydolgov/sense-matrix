type t = RGB of int * int * int | Blank

let of_int = function
  | 0 -> Blank
  | i ->
     let r, g, b =
       i lsr 16,
       i lsr 8 land 0xff,
       i land 0xff
     in
     RGB (r, g, b)

let of_array = function
  | [|0; 0; 0|] -> Blank
  | [|r; g; b|] -> RGB (r, g, b)
  | _ -> failwith "Invalid color array"

let to_array = function
  | RGB (r, g, b) -> [|r; g; b|]
  | Blank -> [|0; 0; 0|]

let to_string = function
  | RGB (r, g, b) -> Printf.sprintf "%02x%02x%02x" r g b
  | Blank -> "      "

let pp fmt color = Format.fprintf fmt "%s" (to_string color)
