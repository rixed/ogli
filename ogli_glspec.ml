open Algen_intf
open Algen_impl
open Glop_base

let debug = false

module Dim = Dim2
module CDim = Dim3

(* Do computations with integers of 18 bits of decimals so
 * that we have integer geometry with enough precision of the
 * projection matrix: *)
module K = Algen_impl.BigNumField (struct let v = 32 end)
(*module K = Algen_impl.FloatField*)

(* Even though we use integers for geometry, glop for opengl
 * still expect float bigarrays: *)

type vertex_array = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t
let make_vertex_array nbv =
    Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout nbv (Dim.v)
let vertex_array_set arr i vec =
    if debug then Format.printf "VA.(%d) <- %a@."
      i (Lr44.array_print (fun fmt v -> Format.fprintf fmt "%f@," (K.to_float v))) vec ;
    Array.iteri (fun c v -> Bigarray.Array2.set arr i c (K.to_float v)) vec

(* OpenGl bindings still expect colors to be float: *)
module KC = Algen_impl.FloatField

type color_array = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t
let make_color_array nbv =
    Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout nbv (CDim.v)
let color_array_set arr i vec =
    Array.iteri (fun c v -> Bigarray.Array2.set arr i c v) vec
