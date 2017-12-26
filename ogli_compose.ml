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
open Ogli

(* The parameter [position] is the position of the parent shape, in case
 * of [over]. *)
let rec display_shape ?(position=Point.origin) image shape =
  let open Point.Infix in
  List.iter (display_shape ~position:shape.Ogli_shape.position image) shape.Ogli_shape.over ;
  Algo.translate_poly (shape.Ogli_shape.position +~ position) shape.Ogli_shape.polys |>
  Algo.rasterize (Img.poke_scanline image shape.Ogli_shape.color shape.Ogli_shape.opacity)

(* Good for one-shot rendering: *)
let display_and_close ?(width=800) ?(height=600) shapes =
  let image = Img.make ~default:Color.white width height in
  List.iter (display_shape image) shapes ;
  Img.open_graph image ;
  Img.draw image ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

(* Good for continuous rendering: *)
type window = Img.t

let open_window ?(background_color=Color.white) width height =
  let img = Img.make ~default:background_color width height in
  Img.open_graph img ;
  img

let renderer win shapes =
  List.iter (display_shape win) shapes ;
  Img.draw win

let close_window _win =
  Graphics.close_graph ()

