type t = RGB of int * int * int | Blank

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

let pp fmt color = Caml.Format.fprintf fmt "%s" (to_string color)
