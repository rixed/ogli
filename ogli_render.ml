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
open K.Infix

(* Set modelview from a camera that sits comfortably at z=0.5.
 * Note that all shape positions are absolute! *)
let set_pos pos =
  G.set_modelview (M.translate pos.(0) pos.(1) (K.of_float ~-.0.5))

let of_polys polys =
  (* TODO: build triangle fans instead *)
  let orig_polys = polys in (* for debug *)
  let polys =
    try Algo.triangulate polys
    with e ->
      Format.printf "Skipping polys %a because of %s\n%s@."
        (Lr44.list_print Poly.print) polys
        (Printexc.to_string e)
        (Printexc.get_backtrace ()) ;
      [] in
  let len, polys =
    List.fold_left (fun (l, ps as prev) p ->
      let len = Poly.length p in
      if len <> 3 then (
        Format.eprintf "Not a triangle: %a@ (triangulated from@ %a)@."
          Poly.print p Poly.print_list orig_polys ;
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
      let x0 = K.to_int pmin.(0)
      and y0 = K.to_int pmin.(1)
      and x1 = K.to_int (K.add K.one pmax.(0))
      and y1 = K.to_int (K.add K.one pmax.(1)) in
      (* As the Bbox captures only the geometry not the rendering, we could
       * occasionally draw outside the bounding box (coordinates are within
       * the bbox but any thick lines/dots or aliasing could cause pixels
       * lying outside of the box to be painted as well. Therefore we
       * enlarge the scissor box a bit: *)
      let x0 = x0-2 and y0 = y0-2 and x1 = x1+2 and y1 = y1+2 in
      G.set_scissor x0 y0 (1+x1-x0) (1+y1-y0) ;
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

let of_text ?(move_to_lower_left=false) text size =
  let word = Word.make text in
  (* font_height should be in pixels: *)
  let _face, face_info = Text_impl.get_face () in
  let font_height = face_info.Freetype.pixel_height in
  let scale = K.of_float (size /. font_height) in
  let move_to_ll =
    if move_to_lower_left then
      Word.lower_left_to_origin word
    else Point.origin in
  let polys =
    (* We must convert to polys in the glyph space regardless of scale
     * or the bezier curves might be wrong. After the scale we should
     * lower the vertex resolution though. *)
    Word.to_polys ~res:K.one word |>
    List.map (fun (pos, polys) ->
      Point.addi pos move_to_ll ;
      Algo.translate_poly pos polys) |>
    List.concat |>
    Algo.scale_poly scale in
  of_polys polys,
  Algo.bbox polys

let shape_of_polys ?on_click ?on_sub_click ?on_hover ?on_drag_start ?on_drag_stop ?on_drag col_polys position children =
  let render, bbox = of_col_polys col_polys in
  Ogli_view.shape (Ogli_shape.make ~position ?on_click ?on_sub_click ?on_hover ?on_drag_start ?on_drag_stop ?on_drag render bbox) children

(* Here position is the position of the beginning of the baseline (the origin of the glyph, where the pen starts).
 * Set move_to_lower_left if the position you pass is actually the lower left corner of where you want that text to be. *)
let shape_of_text ?move_to_lower_left ?on_click ?on_sub_click ?on_hover ?on_drag_start ?on_drag_stop ?on_drag color size text position children =
  let render, bbox = of_text ?move_to_lower_left text size in
  let render = render ~color in
  Ogli_view.shape (Ogli_shape.make ~position ?on_click ?on_sub_click ?on_hover ?on_drag_start ?on_drag_stop ?on_drag render bbox) children

let display = G.swap_buffers

let rec next_event =
  let drag_start = ref None
  and drag_signaled = ref false
  and drag_shifted = ref false
  and click_pos x y w h =
    (* We are given the X11 coordinates but we expect viewport coordinates: *)
    let m = G.get_projection () in
    G.unproject (0, 0, w, h) m x (h - y)
  and min_drag_dist = K.of_int 10 (* how many pixels we have to drag for a drag to be considered *)
  in
  fun ~wait ~on_event ~on_remap ->
    match G.next_event wait with
    | Some (G.Clic (x, y, w, h, shifted)) ->
        let pos = click_pos x y w h in
        (match !drag_start with
        | None ->
            assert (not !drag_signaled) ;
            drag_start := Some pos ;
            drag_shifted := shifted
        (* User managed do get a click through while dragging, using another button.
         * So that's a click event. *)
        | Some _ -> on_event (if shifted then ShiftClick else Click) pos)
    | Some (G.UnClic (x, y, w, h)) ->
        let stop = click_pos x y w h in
        (match !drag_start with
        (* This should not happen, let's ignore that unclick: *)
        | None -> ()
        | Some start ->
            if !drag_signaled then (
              on_event DragStop stop ;
              drag_signaled := false ;
            ) else (
              (* Just a click *)
              on_event (if !drag_shifted then ShiftClick else Click) start
            ) ;
            drag_start := None)
    | Some (G.Move (x, y, w, h)) ->
        let pos = click_pos x y w h in
        (match !drag_start with
        | None ->
            on_event Hover pos
        | Some start ->
            if !drag_signaled then
              on_event Drag pos
            else
              if Point.distance pos start >~ min_drag_dist then (
                on_event DragStart start ;
                on_event Drag pos ;
                drag_signaled := true
              ))
    | Some (G.Resize (w, h)) -> on_remap w h
    | _ -> if wait then next_event ~wait ~on_event ~on_remap

let resize ?(y_down=false) width height =
  let proj =
    let z_near = K.neg K.one and z_far = K.one in
    if y_down then
      M.ortho K.zero (K.of_int width) (K.of_int height) K.zero z_near z_far
    else
      M.ortho K.zero (K.of_int width) K.zero (K.of_int height) z_near z_far in
  G.set_projection proj ;
  G.set_viewport 0 0 width height

let init ?(title="OGli") ?y_down ?double_buffer ?msaa ?font
         width height =
  G.init ~depth:false ~alpha:true ?double_buffer ?msaa title width height ;
  at_exit (fun () -> G.exit ()) ;
  (match font with
  | None -> ()
  | Some file -> Text_impl.font_files := [ file ]) ;
  resize ?y_down width height ;
  set_pos Point.origin ;
  G.clear ~color:C.black ()
