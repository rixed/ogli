module G = Glop_impl.Glop2Dalpha

module K = G.K
module M = G.M (* a GlMatrix of 4x4 *)
module V = G.V
module C = G.C

module Point = Geom_shapes.Point (V)
module Bbox = V.Bbox
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)

(* Some helper to soften the syntax of points, vectors, colors: *)
let p (x : float) (y : float) = [| x ; y |]
let v (x : float) (y : float) = [| x ; y |]
let c (r : float) (g : float) (b : float) = [| r ; g ; b ; 1. |]
let ca (r : float) (g : float) (b : float) (a : float) = [| r ; g ; b ; a |]

(* Instead of polys : Poly.t list, we should have rendering primitives
 * (ie, for Glop, triangulated + array-ized opengl primitives) *)
type shape =
  { color : C.t ; polys : Poly.t list ; over : shape list ;
    (* We have position (TODO: and we could also have axis) so that we could
     * move the shape with a simple operation.
     * This position is relative to the parent though. *)
    position : Point.t }

let shape_print fmt s =
  Format.fprintf fmt "Shape(position=%a)"
    Point.print s.position
