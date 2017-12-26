open Ogli

(* Starting from the full display function, which is what we'd like to write: *)
let flower ~res ~height ~nb_leaves ~base =
  let green = c 0.1 0.9 0.15
  and yellow = c 0.8 0.8 0.1 in
  Ogli_view.fun_of height (fun height ->
    (* First the stem, which depends only on the height: *)
    let stem_height = height *. 0.9 in
    let stem_width = height *. 0.01 in
    let stem =
      Path.start Point.origin |>
      Path.bezier_to (p 0. stem_height)
        [ p (height *. ~-.0.15) (stem_height *. 0.5) ] |>
      Algo.line_of_path ~res ~width:stem_width in
    (* Then the leaves, depends on nb_leaves and height: *)
    let make_unit_leaf () =
      Path.start (p 0. 0.) |>
      Path.bezier_to (p 0. 1.) [p 0.5 0.5 ] |>
      Path.bezier_to (p 0. 0.) [p ~-.0.5 0.5 ] in
    let make_leaf i =
      let i_ratio = float_of_int i /. float_of_int (nb_leaves-1) in
      let h = stem_height *. (0.1 +. 0.6 *. i_ratio) in
      let ang = if i land 1 = 0 then 1.1 else ~-.1.1 in
      make_unit_leaf () |>
      Path.scale (stem_height *. 0.25 *. (1. -. 0.2 *. i_ratio )) |>
      Path.rotate ang |>
      Path.translate (v 0. h) |>
      Algo.poly_of_path ~res |>
      (* fall_on moves the 2nd given poly (here the leaf) in the given
       * direction so that its focused point (here the root of the leaf)
       * touch the 1st given poly (the stem): *)
      (* stem being always bent toward the left *)
      Algo.fall_on ~dir:(v ~-.1. 0.) stem |>
      (* So that all leaf start at the stem center *)
      Poly.translate (v (stem_width *. ~-.0.5) 0.)
    in
    let list_init n f =
      let rec loop prev i =
        if i >= n then List.rev prev else loop (f i :: prev) (i + 1) in
      loop [] 0 in
    let leaves = list_init nb_leaves make_leaf in
    (* Then the bud, for now just a circle, depending only on height: *)
    let bud =
      let radius = height *. 0.1 *. 0.5 in
      Path.circle ~center:(p 0. (stem_height)) radius |>
      Algo.poly_of_path ~res in
    (* Final result: *)
    [ Ogli_view.shape Ogli_shape.{ color = yellow ;
        polys = [ bud ] ; position = base ;
        over = [
          { color = green ;
            polys = stem :: leaves ; position = Point.origin ;
            over = [] } ] } ])

let () =
  (* Window size will be width*height, with (0,0) on the bottom left corner
   * and visible triangles are counter-clockwise. *)
  let width = 800 and height = 600 in
  let flower_height = Ogli_view.make_param "flower height" 10. in
  let res = 0.1 in
  let flower =
    flower ~res ~height:flower_height ~nb_leaves:5 ~base:(p 205. 10.) in
  let renderer =
    Ogli_render.renderer width height in
  let view =
    Ogli_view.make ~double_buffer:true
                   ~pixel_width:width ~pixel_height:height renderer flower
  in
  let rec loop h =
    if h < 1000. then (
      Ogli_render.handle_next_event () ;
      Ogli_view.render view ;
      let h = h +. 2. in
      Ogli_view.param_set flower_height h ;
      loop h
    ) in
  loop flower_height.Ogli_view.value ;
  Unix.sleep 999
