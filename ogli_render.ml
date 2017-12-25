(* An image is made of a composition of polygons, each filled with a single color for now
 * (hopefully flat design is fashionable these days). For future proofing let's use a constructor
 * for the type of things we compose.
 *
 * For efficient diff we need to keep, at every stage of the AST, the old shape and the old sbbox
 * down to individual polygons. And instead of a drawing function we have a diffing function
 * right from the start.
 *
 * So for instance, we do not have a simple `circle x y rad col` but rather a `circle x y rad col some_state`
 *)
open Ogli_geom

type drawable =
  { varr : G.vertex_array ; col : C.t }

let make_drawable polys col =
  let polys =
    Algo.monotonize polys |>
    Algo.triangulate in
  let len =
    List.fold_left (fun l p -> l + Poly.length p) 0 polys in
  let varr = G.make_vertex_array len in
  let _ =
    List.fold_left (Poly.fold_left (fun i point ->
        G.vertex_array_set varr i point ;
        i+1
      )) 0 polys in
  { varr ; col }

let draw_iter t length iteri col prim =
  let len = length t in
  if len <> 3 then (
    Format.printf "NONONO: %a\n%!" Poly.print t ;
    assert (3 = len)) ;
  let varray = G.make_vertex_array (length t) in
  iteri (fun i point -> G.vertex_array_set varray i point) t ;
  G.render prim varray col

let draw_poly col poly =
  (* Use triangle fans instead! *)
  draw_iter poly Poly.length Poly.iteri col G.Triangles

(* The parameter [position] is the position of the parent shape, in case
 * of [over]. *)
let rec draw_shape ?(position=Point.origin) shape =
  let open Point.Infix in
  List.iter (draw_shape ~position:shape.position) shape.over ;
  Algo.translate_poly (shape.position +~ position) shape.polys |>
  List.iter (fun p ->
    let tris = Algo.triangulate [p] in
    if List.for_all (fun t -> Poly.length t = 3) tris then (
      List.iter (draw_poly (G.Uniq shape.color)) tris
    ) else (
      Format.printf "This is not a list of triangles\n" (* ;
      List.iter (Format.printf "%a\n" Poly.print) tris ;
      assert (List.length polys = 1) ;
      Format.printf "Original poly: %a\n"
        Poly.print (List.hd polys) ;
      Format.printf "Monotone version:\n" ;
      List.iter (Format.printf "%a\n" Poly.print) (Algo.monotonize polys) ;
      Format.printf "%!" ;
      assert false) ; *)
    ))

let renderer ?(title="OGli") width height =
  G.init ~depth:false ~alpha:true title width height ;
  at_exit (fun () -> G.exit ()) ;
  let proj =
    let z_near = K.neg K.one and z_far = K.one in
    M.ortho 0. (K.of_int width) 0. (K.of_int height) z_near z_far in
  G.set_projection proj ;
  G.set_viewport 0 0 width height ;
  (* Set modelview from a camera that sits comfortably at z=0.5: *)
  G.set_modelview (M.translate 0. 0. ~-.0.5) ;
  G.clear ~color:C.black () ;
  fun shapes ->
    List.iter draw_shape shapes ;
    G.swap_buffers ()  (* FIXME: we have to diff with buffer N-2! *)

(* TODO: callbacks for various events *)
let handle_next_event () =
  let ev = G.next_event false in
  ignore ev
