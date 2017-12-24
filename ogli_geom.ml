module Kcol = Algen_impl.FloatField
module Color = Oaah_color.Make (Kcol)
module Img = Oaah_image.Make (Color)
module K = Algen_impl.FloatField
module Vector = Algen_vector.Make (K) (struct let v = 2 end)
module Point = Geom_shapes.Point (Vector)
module Bbox = Vector.Bbox
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Dim2 = struct let v = 2 end
module Mat = Algen_matrix.Make (K) (Dim2) (Dim2)

(* Some helper to soften the syntax of points, vectors, colors: *)
let p (x : float) (y : float) = [| x ; y |]
let v (x : float) (y : float) = [| x ; y |]
let c (r : float) (g : float) (b : float) = [| r ; g ; b |]

type shape =
  { opacity : float ; color : Color.t ;
    polys : Poly.t list ; over : shape list ;
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t }

let shape_print fmt s =
  Format.fprintf fmt "Shape(position=%a)"
    Point.print s.position
