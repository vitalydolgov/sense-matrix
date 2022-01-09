open Canvas

let clear = ref false
let angle = ref 0
let sense = ref false

let speclist =
  [("-clear", Arg.Set clear, "Clear matrix");
   ("-angle", Arg.Set_int angle, "Rotate (0, 90, 180, 270)");
   ("-sense", Arg.Set sense, "Display on Sense Hat")
  ]

let from_file f = Layer.from_file f |> Layer.to_matrix

let run (c : canvas) =
  let t_short, t_long = 0.07, 0.97 in
  let h, hs, hss =
    from_file "heart/normal",
    from_file "heart/small",
    from_file "heart/very_small"
  in
  let set s = c#draw s in
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
  Sys.catch_break true;
  Arg.parse speclist (fun _ -> ()) "Usage:";
  if !clear
  then
    let canvas = new canvas (`Sense !angle) in
    canvas#clear
  else
    let canvas =
      if !sense
      then new canvas (`Sense !angle)
      else new canvas `Graphics
    in
    try run canvas with
    | Sys.Break -> canvas#clear
