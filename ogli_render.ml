(* An image is made of a composition of polygons, each filled with a single color for now
 * (hopefully flat design is fashionable these days). For future proofing let's use a constructor
 * for the type of things we compose.
 *
 * For efficient diff we need to keep, at every stage of the AST, the old shape and the old bbox
 * down to individual polygons. And instead of a drawing function we have a diffing function
 * right from the start.
 *
 * So for instance, we do not have a simple `circle x y rad col` but rather a `circle x y rad col some_state`
 *)
open Ogli

(* Set modelview from a camera that sits comfortably at z=0.5.
 * Note that all shape positions are absolute! *)
let set_pos pos =
  G.set_modelview (M.translate pos.(0) pos.(1) ~-.0.5)

let of_polys polys =
  (* TODO: build triangle fans instead *)
  let polys = Algo.triangulate polys in
  let len, polys =
    List.fold_left (fun (l, ps as prev) p ->
      let len = Poly.length p in
      if len <> 3 then (
        Format.eprintf "Not a triangle: %a\n%!" Poly.print p ;
        prev
      ) else (
        l + len, p :: ps
      )) (0, []) polys in
  let varr = G.make_vertex_array len in
  let len' =
    List.fold_left (Poly.fold_left (fun i point ->
        G.vertex_array_set varr i point ;
        i + 1
      )) 0 polys in
  assert (len = len') ;
  fun ~color pos bbox -> (* bbox is already positioned *)
    let do_render () =
      set_pos pos ;
      G.render G.Triangles varr (G.Uniq color) in
    match bbox with
    | None ->
      G.disable_scissor () ;
      do_render ()
    | Some Bbox.Box (pmin, pmax) ->
      let x0 = int_of_float pmin.(0)
      and y0 = int_of_float pmin.(1)
      and x1 = int_of_float (ceil pmax.(0))
      and y1 = int_of_float (ceil pmax.(1)) in
      G.set_scissor x0 y0 (x1-x0) (y1-y0) ;
      do_render ()
    | Some Bbox.Empty -> ()

let of_col_polys col_polys =
  let renderers, bbox =
    List.fold_left (fun (rs, bbs) (color, polys) ->
        of_polys polys ~color :: rs,
        Bbox.union bbs (Algo.bbox polys)
      ) ([], Bbox.empty) col_polys in
  let renderers = List.rev renderers in
  let render pos bbox =
    List.iter (fun r -> r pos bbox) renderers in
  render, bbox

let of_text text size =
  let word = Word.make text in
  (* font_height should be in pixels: *)
  let font_height = Text_impl.face_info.Freetype.pixel_height in
  let scale = size /. font_height in
  let polys =
    Word.to_polys ~res:1. word |>
    List.map (fun (pos, polys) -> Algo.translate_poly pos polys) |>
    List.concat |>
    Algo.scale_poly scale in
  of_polys polys,
  Algo.bbox polys

let init ?(title="OGli") ?(y_down=false) ?(double_buffer=false) width height =
  G.init ~depth:false ~alpha:true ~double_buffer title width height ;
  at_exit (fun () -> G.exit ()) ;
  let proj =
    let z_near = K.neg K.one and z_far = K.one in
    if y_down then
      M.ortho 0. (K.of_int width) (K.of_int height) 0. z_near z_far
    else
      M.ortho 0. (K.of_int width) 0. (K.of_int height) z_near z_far in
  G.set_projection proj ;
  G.set_viewport 0 0 width height ;
  set_pos Point.origin ;
  G.clear ~color:C.black ()

let display = G.swap_buffers

let next_event ~wait ~on_click =
  match G.next_event wait with
  | Some (G.Clic (x, y, w, h)) ->
      (* We are given the X11 coordinates but we expect viewport coordinates: *)
      let m = G.get_projection () in
      let click_pos = G.unproject (0, 0, w, h) m x (h - y) in
      on_click click_pos
  | _ -> ()
  (* TODO: resize *)
