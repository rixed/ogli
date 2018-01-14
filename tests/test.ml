open Lr44
open Ogli
open Ogli_view

let screen_width = Param.make "screen width" 800
let screen_height = Param.make "screen height" 600
let nb_leaves = Param.make "nb leaves" 5
let grow_leaf _ _ = Param.set nb_leaves (nb_leaves.value + 1)
let pick_leaf _ _ =
  if nb_leaves.value > 0 then
    Param.set nb_leaves (nb_leaves.value - 1)


(* Starting from the full display function, which is what we'd like to
 * write: *)
let flower ~res ~height ~jiggling ~base =
  let green = c 0.1 0.9 0.15
  and yellow = c 0.8 0.8 0.1 in
  (* First the stem, which depends only on the height: *)
  let stem_height = height *. 0.9 in
  let stem_width = height *. 0.01 in
  let stem =
    Path.start Point.origin |>
    Path.bezier_to (pf 0. stem_height)
      [ pf (height *. ~-.0.15) (stem_height *. 0.5) ] |>
    Algo.line_of_path ~res ~width:(K.of_float stem_width) in
  (* Then the bud, for now just a circle, depending only on height: *)
  let bud =
    let radius = K.of_float (height *. 0.1 *. 0.5) in
    Path.circle ~center:(pf 0. stem_height) radius |>
    Algo.poly_of_path ~res
  in
  (* So let's do a first object with that: *)
  fun_of nb_leaves (fun nb_leaves ->
    [ Ogli_render.shape_of_polys [ green, [ stem ] ] base [
        Ogli_render.shape_of_polys ~on_click:grow_leaf [ yellow, [ bud ] ] base [
          (* Then the leaves, depends on nb_leaves (cst), height, and
           * jigging (another param): *)
          let make_unit_leaf phi width =
            let extr = pf (0.03 *. cos (1.1 *. phi)) (1. +. (0.03 *. sin (1.37 *. phi))) in
            Path.start Point.origin |>
            Path.bezier_to extr [pf width 0.5 ] |>
            Path.bezier_to Point.origin [pf ~-.width 0.5 ] in
          let make_leaf i =
            let phi = jiggling +. float_of_int i *. 0.1 in
            let i_ratio = float_of_int i /. float_of_int nb_leaves in
            let h = stem_height *. (0.1 +. 0.8 *. i_ratio) in
            let ang = 1.6 -. (1.1 *. i_ratio) +. 0.1 *. cos phi in
            let ang = if i land 1 = 0 then ang else ~-.ang in
            let stem_height = stem_height *. (1. -. 0.5 *. i_ratio) in
            let width = 0.8 -. 0.5 *. i_ratio in
            make_unit_leaf phi width |>
            Path.scale (K.of_float (stem_height *. 0.25 *. (1. -. 0.2 *. i_ratio ))) |>
            Path.rotate (K.of_float ang) |>
            Path.translate (vf 0. h) |>
            Algo.poly_of_path ~res |>
            (* fall_on moves the 2nd given poly (here the leaf) in the given
             * direction so that its focused point (here the root of the leaf)
             * touch the 1st given poly (the stem): *)
            (* stem being always bent toward the left *)
            Algo.fall_on ~dir:(vf ~-.1. 0.) stem |>
            (* So that all leaves start at the stem center *)
            Poly.translate (vf (stem_width *. ~-.0.5) 0.)
          in
          let leaves = list_init nb_leaves make_leaf
          in
          Ogli_render.shape_of_polys ~on_click:pick_leaf [ green, leaves ] base [] ]]])

let () =
  (* Window size will be width*height, with (0,0) on the bottom left corner
   * and visible triangles are counter-clockwise. *)
  let double_buffer = true and msaa = true in
  (* This is the user's job to init since we could have plenty of views. *)
  Ogli_render.init ~title:"OGLI test" ~double_buffer ~msaa
                   screen_width.value screen_height.value ;
  (* Parameters: *)
  let flower_height_phase = Param.make "flower height phase" 0. in
  let jiggling = Param.make "leaves jiggling phase" 0. in
  (* Event handler: *)
  let quit = ref false in
  let on_click _ _ = quit := true in
  (* Picture depending on those parameters: *)
  let pic =
    fun_of screen_width (fun screen_w ->
      [ fun_of screen_height (fun screen_h ->
          let background =
            Path.rect Point.origin (pi screen_w screen_h) |>
            Algo.poly_of_path ~res:K.one (* unused *) in
          [ Ogli_render.shape_of_polys [ C.white, [ background ] ] Point.origin [
              fun_of jiggling (fun jiggling ->
                let size = 14. +. cos (jiggling *. 0.13) in
                let pos = pf (300. +. 10. *. cos (7.3 *. jiggling))
                             (500. +. 3. *. sin (5.9 *. jiggling)) in
                [ Ogli_render.shape_of_text ~on_click C.black size "Hello WORLD!" pos [] ;
                  fun_of flower_height_phase (fun height_phase ->
                    let height = float_of_int screen_h *.
                                 (0.75 +. 0.004 *. cos height_phase) in
                   [ flower ~res:(K.of_float 0.1) ~height ~jiggling
                            ~base:(pf 205. 10.)])])]])]) in
  let view =
    make ~double_buffer ~width:screen_width ~height:screen_height pic
  in
  while not !quit do
    render view ;
    next_event view (Ogli_render.next_event ~wait:false)
               Ogli_render.resize ;
    Ogli_render.display () ;
    Param.set flower_height_phase (flower_height_phase.value +. 0.031) ;
    Param.set jiggling (jiggling.value +. 0.0023)
  done
