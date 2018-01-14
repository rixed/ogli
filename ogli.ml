(*module G = Glop_impl.Glop2Dalpha*)
(*module G = Glop_impl.Glop2D*)
(* Let's make our own for 2D graphics with integers: *)
module G = Glop_impl.MakeCustom (Ogli_glspec)

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
let p (x : K.t) (y : K.t) : Point.t = [| x ; y |]
let pf (x : float) (y : float) : Point.t = [| K.of_float x ; K.of_float y |]
let pi x y : V.t = [| K.of_int x ; K.of_int y |]
let v (x : K.t) (y : K.t) : V.t = [| x ; y |]
let vf (x : float) (y : float) : V.t = [| K.of_float x ; K.of_float y |]
let c (r : float) (g : float) (b : float) : C.t = [| r ; g ; b (*; 1. *) |]
(*let ca (r : float) (g : float) (b : float) (a : float) : C.t = [| r ; g ; b ; a |]*)

type event = Click | ShiftClick | DragStart | Drag | DragStop | Hover

let string_of_event = function
  | Click -> "Click"
  | ShiftClick -> "ShiftClick"
  | DragStart -> "DragStart"
  | Drag -> "Drag"
  | DragStop -> "DragStop"
  | Hover -> "Hover"

let print_event fmt event =
  Format.fprintf fmt "%s" (string_of_event event)
