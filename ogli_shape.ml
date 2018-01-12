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
    on_drag_start : (Point.t -> unit) option ;
    on_drag : (Point.t -> unit) option ;
    on_drag_stop : (Point.t -> unit) option ;
    (* TODO: on_focus, on_input... *)
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t }

let make ?on_click ?on_sub_click ?on_hover
         ?on_drag_start ?on_drag ?on_drag_stop
         ?(position = Point.origin)
         render bbox =
  { render ; bbox ; on_click ; on_sub_click ; on_hover ;
    on_drag_start ; on_drag ; on_drag_stop ; position }

let handler_for_event s = function
  | Click -> Lr44.option_map (fun f -> f false) s.on_click
  | ShiftClick -> Lr44.option_map (fun f -> f true) s.on_click
  | DragStart -> s.on_drag_start
  | Drag -> s.on_drag
  | DragStop -> s.on_drag_stop
  | Hover -> s.on_hover

let print fmt s =
  Format.fprintf fmt "@[Shape(@[bbox=%a"
    Bbox.print (Bbox.translate s.bbox s.position) ;
  if s.on_click <> None then Format.fprintf fmt "@;on_click" ;
  Format.fprintf fmt "@])@]"
