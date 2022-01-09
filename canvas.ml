type t = Graphics | Sense of Sense.matrix

class canvas init =
  let backend = match init with
    | `Graphics -> Graph.open_canvas (); Graphics
    | `Sense angle -> Sense (new Sense.matrix angle)
  in
  object
    method draw matrix = match backend with
      | Graphics -> Graph.draw matrix
      | Sense m -> m#set_state matrix; m#push

    method clear = match backend with
      | Graphics -> Graph.clear ()
      | Sense m -> m#clear
  end
