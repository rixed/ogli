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

let rec display_shape ?(position=Point.origin) image shape =
  let open Point.Infix in
  List.iter (display_shape ~position:shape.position image) shape.over ;
  Algo.translate_poly (shape.position +~ position) shape.polys |>
  Algo.rasterize (Img.poke_scanline image shape.color shape.opacity)

let display shapes =
  let image = Img.make ~default:Color.white 800 600 in
  List.iter (display_shape image) shapes ;
  Img.open_graph image ;
  Img.draw image ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

(* Example: *)

(* Starting from the full display function, which is what we'd like to write: *)
let flower ~height ~nb_leaves ~base =
  let green = c 0.1 0.9 0.15
  and yellow = c 0.8 0.8 0.1 in
  (* First the stem, which depends only on the height: *)
  let stem_height = height *. 0.9 in
  let res = height *. 0.05 |> min 3. |> max 1. in
  Format.printf "res = %a@." K.print res ;
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
  { opacity = 0.5 ; color = yellow ;
    polys = [ bud ] ; position = base ;
    over = [
      { opacity = 1. ; color = green ;
        polys = stem :: leaves ; position = Point.origin ;
        over = [] } ] }
