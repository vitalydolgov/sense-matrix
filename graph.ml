open Graphics

let size = 30
let gap = 2

let open_canvas () =
  let canvas_w, canvas_h =
    Matrix.dimx * (size + gap),
    Matrix.dimy * (size + gap)
  in
  open_graph @@ Printf.sprintf " %dx%d" canvas_w canvas_h

let rotate matrix =
  let init = List.init (Array.length matrix.(0)) (fun _ -> []) in
  Array.fold_left (fun acc row ->
      List.map2 (fun x y -> x :: y) (Array.to_list row) acc) init matrix

let draw_aux i j color =
  set_color (match color with
             | Color.RGB color -> color
             | Blank -> white);
  let x, y = i * (size + gap), j * (size + gap) in
  fill_rect x y size size

let draw matrix =
  List.iteri (fun i row ->
      List.iteri (fun j color ->
          draw_aux i j color) row) (rotate matrix)

let clear = clear_graph
