open Ogli

(* A shape is just a set of such rendered, organised hierarchically for
 * convenience. *)
type t =
  { render : Point.t -> Bbox.t option -> unit ;
    bbox : Bbox.t ; (* regardless of position *)
    on_click : (unit -> unit) option ;
    (* TODO: on_focus, on_input... *)
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t }

let print fmt s =
  Format.fprintf fmt "@[Shape(@[bbox=%a"
    Bbox.print (Bbox.translate s.bbox s.position) ;
  if s.on_click <> None then Format.fprintf fmt "@;on_click" ;
  Format.fprintf fmt "@])@]"
