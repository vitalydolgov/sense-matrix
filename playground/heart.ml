open Sense

let clear = ref false
let angle = ref 0

let speclist =
  [("-clear", Arg.Set clear, "Clear LED matrix");
   ("-angle", Arg.Set_int angle, "Rotate (0, 90, 180, 270)")]

let from_file f = Layer.of_file f |> Layer.to_matrix

let run (m : matrix) =
  let t_short, t_long = 0.07, 0.97 in
  let h, hs, hss =
    from_file "heart/normal",
    from_file "heart/small",
    from_file "heart/very_small"
  in
  let set s = m#set_state s; m#push in
  let delay = Unix.sleepf in
  let beat () =
    set h;
    delay t_long;
    set hs;
    delay t_short;
    set hss;
    delay t_short;
    set hs;
    delay t_short;
    set hs;
    delay t_short;
    set hs;
    delay t_short;
    set hss;
    delay t_short;
    set hs;
    delay t_short
  in
  while true do
    beat ();
  done

let _ =
  Arg.parse speclist (fun _ -> ()) "Usage:";
  let matrix = new matrix !angle in
  if !clear
  then matrix#clear
  else run matrix
