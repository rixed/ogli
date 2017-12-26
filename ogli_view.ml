open Ogli

(* An Ogli_shape.t is a picture (an assemblage of colored polys).
 * Now we want to generate parameterized pictures, and then change the
 * parameters and have a function that updates the picture when it needs to
 * be, and update the canvas as parsimoniously as possible.  So we will build
 * a difftree of shapes, with explicit functions of parameters here and
 * there, and use the difftree to compute a sequence of commands to update
 * the canvas when parameters change: First, we recompute the new tree
 * keeping everything we can and recomputing only what depends on updated
 * parameters, then we call the diff function between the former tree and the
 * new one, building a sequence of update operations (ie: nothing but a
 * sequence of polygons to draw), that we optimise and render.  *)

(* The non polymorphic part of a parameter: *)
type param_desc =
  { name : string ; (* TODO: get rid of this *)
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

let param_set p v =
  p.desc.last_changed <- clock () ;
  p.value <- v

type shape_tree = item Ogli_difftree.t
and item = Shape of Ogli_shape.t
          | Function of {
              (* We keep a reference to the param desc so we know when
               * the param is changed. *)
              param : param_desc ;
              f : (unit -> shape_tree list) ;
              last_refresh : int }

let fun_of p f =
  let head = Function {
    param = p.desc ;
    f = (fun () -> f p.value) ;
    last_refresh = 0 } in
  Ogli_difftree.make head []

let shape s =
  let head = Shape s in
  Ogli_difftree.make head []

(* Now a "window" must have essentially a difftree and a set of params: *)

type t =
  { mutable tree : shape_tree ;
    double_buffer : bool ;
    mutable past_tree : shape_tree option ; (* if double buffer *)
    mutable last_render : int (* date after last render *) ;
    min_coord : V.t ;
    max_coord : V.t ;
    pixel_width : int ;
    pixel_height : int ;
    (* There should be no such thing as a background color.
     * The background is undefined, and if we want one we
     * should start by displaying a rectangle over the window. *)
    background_color : C.t ;
    renderer : Ogli_shape.t list -> unit }

let make ?(min_coord = p 0. 0.) ?(max_coord = p 1. 1.)
         ?(pixel_width = 800) ?(pixel_height = 800)
         ?(background_color = C.white) ?(double_buffer = false)
         renderer tree =
  { tree ; double_buffer ; past_tree = None ;
    min_coord ; max_coord ; pixel_width ; pixel_height ;
    last_render = 0 ; background_color ; renderer }

(* Drawing commands to update the canvas: *)
type cmd = Add of Ogli_shape.t | Del of Ogli_shape.t

let cmd_print fmt = function
  | Add s -> Format.fprintf fmt "Add %a" Ogli_shape.print s
  | Del s -> Format.fprintf fmt "Del %a" Ogli_shape.print s

let deletion t s =
  let open Ogli_shape in
  let sb = sbbox s in
  let polys =
    let res = K.one in (* unused *)
    List.map (fun bb ->
      Path.of_bbox bb |>
      Algo.poly_of_path ~res) sb.bboxes in
  { color = t.background_color ;
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
  (* No. We should redraw everything but this part of the tree, clipped by
   * the bounding box of this item. Therefore, we should also be given the
   * tree, or an iterator over its "continuation". *)
  match item with
  | Function _ -> cmd
  | Shape s -> Del s :: cmd

let diff = Ogli_difftree.diff del add

let format_list pp fmt lst =
  Format.fprintf fmt "[" ;
  List.iter (fun x ->
    Format.fprintf fmt "@[%a@]@," pp x) lst ;
  Format.fprintf fmt "]"

let render t =
  (* update the functions children whenever their param have changed: *)
  let next_tree = Ogli_difftree.map t.tree (function
    | Function { param ; f ; last_refresh }, key
      when last_refresh < param.last_changed ->
        let new_head =
          Some (Function { param ; f ; last_refresh = clock () }, key) in
        Some (new_head, f ())
    | _ -> None) in
  (* Compute the drawing command required to update the canvas: *)
  let cmds =
    if t.double_buffer then (
      let last_but_one = t.past_tree in
      t.past_tree <- Some t.tree ;
      match last_but_one with
      | None -> []
      | Some lb1 ->
        diff lb1 next_tree []
    ) else (
      diff t.tree next_tree []
    ) in
  t.tree <- next_tree ;
  t.last_render <- clock () ;
  polys_of_cmds t cmds |>
  t.renderer
