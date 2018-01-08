open Ogli

(* A shape is just a set of such rendered, organised hierarchically for
 * convenience. *)
type t =
  { render : Point.t -> Bbox.t option -> unit ;
    bbox : Bbox.t ; (* regardless of position *)
    on_click : (Point.t -> unit) option ;
    on_hover : (Point.t -> unit) option ;
    on_unhover : (Point.t -> unit) option ;
    on_drag_start : (Point.t -> unit) option ;
    on_drag : (Point.t -> unit) option ;
    on_drag_stop : (Point.t -> unit) option ;
    (* TODO: on_focus, on_input... *)
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t }

let handler_for_event s = function
  | Click -> s.on_click
  | DragStart -> s.on_drag_start
  | Drag -> s.on_drag
  | DragStop -> s.on_drag_stop
  | HoverStart -> s.on_hover
  | HoverStop -> s.on_unhover

let print fmt s =
  Format.fprintf fmt "@[Shape(@[bbox=%a"
    Bbox.print (Bbox.translate s.bbox s.position) ;
  if s.on_click <> None then Format.fprintf fmt "@;on_click" ;
  Format.fprintf fmt "@])@]"
