open Ogli

(* Starting from the full display function, which is what we'd like to
 * write: *)
let flower ~res ~height ~jiggling ~nb_leaves ~base =
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
    (* Then the bud, for now just a circle, depending only on height: *)
    let bud =
      let radius = height *. 0.1 *. 0.5 in
      Path.circle ~center:(p 0. (stem_height)) radius |>
      Algo.poly_of_path ~res
    in
    (* So let's do a first object with that: *)
    [ Ogli_render.shape_of_polys [ green, [ stem ] ; yellow, [ bud ] ] base [
      (* Then the leaves, depends on nb_leaves (cst), height, and
       * jigging (another param): *)
      Ogli_view.fun_of jiggling (fun jiggling ->
        let make_unit_leaf phi =
          let extr = p (0.03 *. cos (1.1 *. phi)) (1. +. (0.03 *. sin (1.37 *. phi))) in
          Path.start (p 0. 0.) |>
          Path.bezier_to extr [p 0.5 0.5 ] |>
          Path.bezier_to (p 0. 0.) [p ~-.0.5 0.5 ] in
        let make_leaf i =
          let phi = jiggling +. float_of_int i *. 0.1 in
          let i_ratio = float_of_int i /. float_of_int (nb_leaves-1) in
          let h = stem_height *. (0.1 +. 0.6 *. i_ratio) in
          let ang = 1.1 +. 0.1 *. cos phi in
          let ang = if i land 1 = 0 then ang else ~-.ang in
          make_unit_leaf phi |>
          Path.scale (stem_height *. 0.25 *. (1. -. 0.2 *. i_ratio )) |>
          Path.rotate ang |>
          Path.translate (v 0. h) |>
          Algo.poly_of_path ~res |>
          (* fall_on moves the 2nd given poly (here the leaf) in the given
           * direction so that its focused point (here the root of the leaf)
           * touch the 1st given poly (the stem): *)
          (* stem being always bent toward the left *)
          Algo.fall_on ~dir:(v ~-.1. 0.) stem |>
          (* So that all leaves start at the stem center *)
          Poly.translate (v (stem_width *. ~-.0.5) 0.)
        in
        let list_init n f =
          let rec loop prev i =
            if i >= n then List.rev prev else loop (f i :: prev) (i + 1) in
          loop [] 0 in
        let leaves = list_init nb_leaves make_leaf
        in
        [ Ogli_render.shape_of_polys [ green, leaves ] base [] ])
      ] ])

let () =
  (* Window size will be width*height, with (0,0) on the bottom left corner
   * and visible triangles are counter-clockwise. *)
  let width = 800 and height = 600 and double_buffer = true in
  (* This is the user's job to init since we could have plenty of views. *)
  Ogli_render.init ~double_buffer width height ;
  (* Parameters: *)
  let flower_height = Ogli_view.Param.make "flower height" 10. in
  let jiggling = Ogli_view.Param.make "leaves jiggling phase" 0. in
  (* Event handler: *)
  let quit = ref false in
  let on_click () = quit := true in
  (* Picture depending on those parameters: *)
  let pic =
    let background =
      Path.rect (p 0. 0.) (pi width height) |>
      Algo.poly_of_path ~res:K.one (* unused *) in
    Ogli_render.shape_of_polys ~on_click [ C.white, [ background ] ] Point.origin [
      Ogli_view.fun_of jiggling (fun jiggling ->
        let size = 10. +. cos (jiggling *. 0.13) in
        let pos = p (300. +. cos jiggling) (200. +. sin jiggling) in
        [ Ogli_render.shape_of_text C.black size "Hello WORLD!" pos [] ]) ;
      flower ~res:0.1 ~height:flower_height ~jiggling
             ~nb_leaves:5 ~base:(p 205. 10.) ] in
  let view =
    Ogli_view.make ~double_buffer
                   ~pixel_width:width ~pixel_height:height pic
  in
  let rec loop h =
    Ogli_view.render view ;
    Ogli_view.next_event view (Ogli_render.next_event ~wait:false) ;
    Ogli_render.display () ;
    let h = h +. 2. in
    Ogli_view.Param.set flower_height h ;
    Ogli_view.Param.set jiggling (jiggling.value +. 0.1) ;
    if not !quit then loop h
  in
  loop flower_height.Ogli_view.Param.value
