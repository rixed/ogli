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

let param name value =
  { desc = { name ; last_changed = clock () } ; value }

let param_set p v =
  p.desc.last_changed <- clock () ;
  p.value <- v

type shape_tree = item Ogli_difftree.t
and item = Shape of Ogli_shape.t
         | Function of {
            (* We keep a reference to the param desc so we know when
             * the param is changed. *)
            param : Param.desc ;
            f : (unit -> Constraint.t list * shape_tree list) ;
            last_refresh : int }

let fun_of p f =
  let head = Function {
    param = p.desc ;
    f = (fun () -> f p.value) ;
    last_refresh = 0 } in
  Ogli_difftree.make head []

let shape s l =
  let head = Shape s in
  Ogli_difftree.make head l

(* In painting order, aka from root to leaves: *)
let rec iter_shapes f tree =
  Ogli_difftree.iter_breadth_first (function
    | Function _ -> () (* we assume the tree is up to date *)
    | Shape s -> f s) tree

(* Now a "window" must have essentially a difftree and a set of params: *)

type t =
  { mutable tree : shape_tree ;
    double_buffer : bool ;
    mutable past_tree : shape_tree option ; (* if double buffer *)
    min_coord : V.t ;
    max_coord : V.t ;
    pixel_width : int ;
    pixel_height : int ;
    mutable frame_num : int }

let make ?(min_coord = p 0. 0.) ?(max_coord = p 1. 1.)
         ?(pixel_width = 800) ?(pixel_height = 800)
         ?(double_buffer = false)
         tree =
  { tree ; double_buffer ; past_tree = None ;
    min_coord ; max_coord ; pixel_width ; pixel_height ;
    frame_num = 0 }

(* Drawing commands to update the canvas: *)
type cmd = Add of Ogli_shape.t | Del of Ogli_shape.t

let cmd_print fmt = function
  | Add s -> Format.fprintf fmt "Add %a" Ogli_shape.print s
  | Del s -> Format.fprintf fmt "Del %a" Ogli_shape.print s

let delete t s =
  let open Ogli_shape in
  (*Format.printf "delete bbox %a at pos %a\n%!"
    Bbox.print s.bbox Point.print s.position ;*)
  (* Render the whole tree (FIXME: not what we are going to redraw later,
   * see later commands in cmds) in the shape bbox: *)
  let bbox = Some (Bbox.translate s.bbox s.position) in
  (* Note: by the time we are called, t.tree is the new tree *)
  iter_shapes (fun s -> s.render s.position bbox) t.tree

(* [cmds] is the reverted list of drawing commands to be executed in order
 * to move the canvas from what it is to what it should be. We should
 * optimise this and then compute the polys that actually needs to be
 * drawn.
 * A simpler approach is to just to everything ; but the deletion is *very*
 * expensive. *)
let render_cmds t cmds =
  (* TODO: optimize cmds here so that we merge what can be merged *)
  List.rev cmds |>
  List.iter (function
    | Add s ->
      s.render s.position None
    | Del s -> delete t s)

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
  let prev_tree =
    if t.double_buffer then (
      let tree =
        match t.past_tree with
        | None -> Ogli_difftree.empty []
        | Some tree -> tree in
      t.past_tree <- Some t.tree ;
      if t.frame_num < 2 then Ogli_difftree.empty [] else tree
    ) else (
      if t.frame_num < 1 then Ogli_difftree.empty [] else t.tree
    ) in
  let cmds = Ogli_difftree.diff del add prev_tree next_tree [] in
  t.tree <- next_tree ;
  render_cmds t cmds ;
  t.frame_num <- t.frame_num + 1
