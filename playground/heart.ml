open Sense

let normalize string =
  Str.(global_substitute (regexp " ") (fun _ -> "")) string
  |> String.to_array


let heart =
  let array =
    normalize

      "0 0 0 0 0 0 0 0\
       0 1 1 0 0 1 1 0\
       1 1 1 1 1 1 1 1\
       1 1 1 1 1 1 1 1\
       0 1 1 1 1 1 1 0\
       0 0 1 1 1 1 0 0\
       0 0 0 1 1 0 0 0\
       0 0 0 0 0 0 0 0"

    |> Array.map ~f:(function
           | '0' -> Color.Blank
           | _ -> Color.RGB (255, 0, 10))
  in
  Matrix.of_array (`Colors array)


let small_heart =
  let array =
    normalize

      "0 0 0 0 0 0 0 0\
       0 0 0 0 0 0 0 0\
       0 0 1 0 0 1 0 0\
       0 1 1 1 1 1 1 0\
       0 0 1 1 1 1 0 0\
       0 0 0 1 1 0 0 0\
       0 0 0 0 0 0 0 0\
       0 0 0 0 0 0 0 0"

    |> Array.map ~f:(function
           | '0' -> Color.Blank
           | _ -> Color.RGB (150, 0, 80))
  in
  Matrix.of_array (`Colors array)


let very_small_heart =
  let array =
    normalize

      "0 0 0 0 0 0 0 0\
       0 0 0 0 0 0 0 0\
       0 0 0 0 0 0 0 0\
       0 0 1 1 1 1 0 0\
       0 0 1 1 1 1 0 0\
       0 0 0 0 0 0 0 0\
       0 0 0 0 0 0 0 0\
       0 0 0 0 0 0 0 0"

    |> Array.map ~f:(function
           | '0' -> Color.Blank
           | _ -> Color.RGB (120, 0, 100))
  in
  Matrix.of_array (`Colors array)


let run (m : matrix) =
  let t_short, t_long = 0.07, 0.97 in
  let h, hs, hss = heart, small_heart, very_small_heart in
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
