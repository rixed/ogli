open Ogli

(* A shape is just a set of such rendered, organised hierarchically for
 * convenience. *)
type t =
  { render : Point.t -> Bbox.t option -> unit ;
    bbox : Bbox.t ; (* regardless of position *)
    on_click : (bool -> Point.t -> unit) option ;
    (* on_sub_click is called even when the click is for a deeper component.
     * When set, on_click is called only for clicks falling on this shape. *)
    on_sub_click : (bool -> Point.t -> unit) option ;
    on_hover : (Point.t -> unit) option ;
    on_drag_start : (bool -> Point.t -> unit) option ;
    on_drag : (Point.t -> unit) option ;
    on_drag_stop : (Point.t -> unit) option ;
    (* TODO: on_focus, on_input... *)
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t ;
    track : bool }

let print fmt s =
  Format.fprintf fmt "@[Shape(@[bbox=%a@ pos=%a@])@]"
    Bbox.print s.bbox
    Point.print s.position ;
  if s.on_click <> None then Format.fprintf fmt "@;on_click" ;
  Format.fprintf fmt "@])@]"

let make ?(track=false) ?on_click ?on_sub_click ?on_hover
         ?on_drag_start ?on_drag ?on_drag_stop
         ?(position = Point.origin)
         render bbox =
  let s = { track ; render ; bbox ; on_click ; on_sub_click ; on_hover ;
    on_drag_start ; on_drag ; on_drag_stop ; position } in
  if track then Format.printf "Create %a@." print s ;
  s

let handler_for_event s = function
  | Click -> Lr44.option_map (fun f -> f false) s.on_click
  | ShiftClick -> Lr44.option_map (fun f -> f true) s.on_click
  | DragStart -> Lr44.option_map (fun f -> f false) s.on_drag_start
  | ShiftDragStart -> Lr44.option_map (fun f -> f true) s.on_drag_start
  | Drag -> s.on_drag
  | DragStop -> s.on_drag_stop
  | Hover -> s.on_hover
