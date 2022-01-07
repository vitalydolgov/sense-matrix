open Pyops

let _ = Py.initialize ()

class matrix angle =
  let sense_hat = Py.import "sense_hat" in
  let sense = sense_hat.&("SenseHat")[||] in
  let _ =
      let angle' = Py.Int.of_int angle in
      let redraw = Py.Bool.of_bool false in
      sense.&("set_rotation")[|angle'; redraw|]
  in
  object (self)
    val mutable matrix = Matrix.empty ()

    method get_state = matrix
    method set_state m = matrix <- m

    method set_pixel i j color = matrix.(i).(j) <- color
    method get_pixel i j = matrix.(i).(j)

    method pull =
      let pixels = sense.&("get_pixels")[||] in
      matrix <- Matrix.of_pyarray pixels

    method push =
      let pixels = Matrix.to_pyarray matrix in
      sense.&("set_pixels")[|pixels|] |> ignore

    method clear =
      matrix <- Matrix.empty ();
      self#push

    method low_light state =
      let state' = Py.Bool.of_bool state in
      sense.@$("low_light") <- state'

    initializer self#pull
  end
