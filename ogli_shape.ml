open Ogli

(* Anything that can be rendered with a color and according to a clip-box is
 * a primitive we can work with: *)
type primitive = C.t -> Ogli_sbbox.t -> unit

(* Instead of polys : Poly.t list, we should have rendering primitives
 * (ie, for Glop, triangulated + array-ized opengl primitives) *)
type t =
  { color : C.t ; polys : Poly.t list ; over : t list ;
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t }

let print fmt s =
  Format.fprintf fmt "Shape(position=%a)"
    Point.print s.position

(* Returns the bbox in the same space as the shape (ie. same bbox regardless
 * if the shape position. *)
let rec sbbox s =
  let open Ogli_sbbox in
  let sb = singleton (Algo.bbox s.polys) in
  List.fold_left (fun sb s' ->
    (* We still need to position the sub-shapes bboxes: *)
    let sb' = sbbox s' |>
              translate s'.position in
    merge sb sb') sb s.over
