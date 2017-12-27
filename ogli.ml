module G = Glop_impl.Glop2Dalpha

module K = G.K
module M = G.M (* a GlMatrix of 4x4 *)
module V = G.V
module C = G.C

(* Pt, Bb, Py, Ph, Al ? *)
module Point = Geom_shapes.Point (V)
module Bbox = Point.Bbox
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)

module Glyph = Text_impl.Glyph (Poly) (Path)
module Word = Text_impl.Word (Glyph)

(* Some helper to soften the syntax of points, vectors, colors: *)
let p (x : float) (y : float) = [| x ; y |]
let v (x : float) (y : float) = [| x ; y |]
let c (r : float) (g : float) (b : float) = [| r ; g ; b ; 1. |]
let ca (r : float) (g : float) (b : float) (a : float) = [| r ; g ; b ; a |]
