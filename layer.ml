type t = char array * (char * Color.t) list

let normalize string =
  let re = Re.Perl.compile_pat "\\s+" in
  Re.replace re ~f:(fun _ -> "") string

let filename name =
  let re = Re.Perl.compile_pat ".*\\.layer$" in
  if Re.matches re name <> []
  then name
  else name ^ ".layer"

let parse_matrix lines =
  List.fold_left ( ^ ) "" lines
  |> normalize
  |> String.to_seq
  |> Array.of_seq

let mapping_of_string s =
  let re = Re.Perl.compile_pat "(.{1})=([\\da-f]{6})" in
  let m = Re.all re s |> List.hd in
  let char, value =
    Re.Group.get m 1 |> Fun.flip String.get 0,
    "0x" ^ Re.Group.get m 2 |> int_of_string |> Color.of_int
  in
  char, value

let parse_legend lines =
  List.map mapping_of_string lines

let read_lines filename =
  let ic = open_in filename in
  let rec loop lines =
    try
      (input_line ic) :: lines |> loop
    with
    | End_of_file ->
       close_in ic;
       List.rev lines
  in
  loop []

let from_file name =
  let filename = filename name in
  let lines = read_lines filename in
  let lines_l, lines_m =
    List.partition (fun x -> String.contains x '=') lines
  in
  let matrix = parse_matrix lines_m in
  let legend = parse_legend lines_l in
  matrix, legend

let to_matrix layer =
  let colors =
    Array.map (fun x -> List.assoc x (snd layer)) (fst layer)
  in
  Matrix.of_array (`Colors colors)
