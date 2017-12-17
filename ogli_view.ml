open Ogli_geom

(* An Ogli_geom.shape is a picture (an assemblage of colored polys).
 * Now we want to generate bigger picture according to parameters, and then
 * change the parameters and have a function that updates the picture when it
 * needs to be, and update the canvas as parsimoniously as possible.
 * So we will build a difftree of shapes, with explicit functions of parameters
 * here and there, and use the difftree to compute a sequence of commande to
 * update the canvas when parameters change:
 * First, we recompute the new tree keeping everything we can and recomputing
 * only what depends on updated parameters, then we call the diff function
 * between the former tree and the new one, building a sequence of update
 * operations (ie: nothing but a sequence of polygons to draw), that we
 * optimise and render.
 * *)

(* The non polymorphic part of a parameter: *)
type param_desc =
  { name : string ;
    mutable last_changed : int }

(* A parameter is essentially a named ref cell.
 * Functions keep a reference to a param_desc param, so you cannot
 * delete params dynamically. *)
type 'a param = { desc : param_desc ; mutable value : 'a }

let clock =
  let seq = ref 0 in
  fun () ->
    incr seq ;
    !seq

let make_param name value =
  { desc = { name ; last_changed = clock () } ; value }

type item = Shape of shape
          | Function of param_desc * (unit -> shape_tree list)
and shape_tree = item Ogli_difftree.t

let fun_of p f =
  let head = Function (p.desc, fun () -> f p.value) in
  Ogli_difftree.make head []

let share s =
  let head = Shape s in
  Ogli_difftree.make head []

(* Now a "window" must have essentially a difftree and a set of params: *)

type t =
  { mutable tree : shape_tree ;
    mutable last_render : int (* date after last render *) ;
    min_coord : Vector.t ;
    max_coord : Vector.t ;
    pixel_width : int ;
    pixel_height : int ;
    background_color : Color.t }

let make ?(min_coord = p 0. 0.) ?(max_coord = p 1. 1.)
         ?(pixel_width = 800) ?(pixel_height = 800)
         ?(background_color = Color.black) () =
  { tree = Ogli_difftree.empty [] ;
    min_coord ; max_coord ; pixel_width ; pixel_height ;
    last_render = 0 ; background_color }

(* Drawing commands to update the canvas: *)
type cmd = Add of shape | Del of shape

let deletion t s =
  let sb = Ogli_sbbox.of_shape s in
  let polys =
    let res = K.one in (* unused *)
    List.map (fun bb ->
      Path.of_bbox bb |>
      Algo.poly_of_path ~res) sb.bboxes in
  { opacity = 1. ; color = t.background_color ;
    polys ; over = [] ; position = s.position }
  (* TODO: also redraw everything but s, clipped by bb *)

(* [cmds] is the reverted list of drawing commands to be executed in order
 * to move the canvas from what it is to what it should be. We should
 * optimise this and then compute the polys that actually needs to be
 * drawn.
 * A simpler approach is to just to eveything ; but the deletion is *very*
 * expensive. *)
let polys_of_cmds t cmds =
  List.rev cmds |>
  List.map (function
    | Add s -> s
    | Del s -> deletion t s)

let add item cmd =
  match item with
  | Function _ -> cmd
  | Shape s -> Add s :: cmd

let del item cmd =
  match item with
  | Function _ -> cmd
  | Shape s -> Del s :: cmd

let diff = Ogli_difftree.diff del add

let render t tree =
  let last_render = clock () in
  (*
    (* update the shapes where params have changed: *)
    let tree = t.tree (* TODO *) in
  *)
  (* Compute the drawing command required to update the canvas: *)
  let cmds = diff t.tree tree [] in
  t.tree <- tree ;
  t.last_render <- last_render ;
  polys_of_cmds t cmds |>
  Ogli_compose.display
