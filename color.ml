type t = RGB of int | Blank

let of_int = function
  | 0 -> Blank
  | i -> RGB i

let of_array = function
  | [|0; 0; 0|] -> Blank
  | [|r; g; b|] -> RGB (r lsl 16 + g lsl 8 + b)
  | _ -> failwith "Invalid color array"

let to_array = function
  | RGB i ->
     let r, g, b =
       i lsr 16,
       i lsr 8 land 0xff,
       i land 0xff
     in
     [|r; g; b|]
  | Blank -> [|0; 0; 0|]

let to_string = function
  | RGB i -> Printf.sprintf "%06x" i
  | Blank -> "      "

let pp fmt color = Format.fprintf fmt "%s" (to_string color)
