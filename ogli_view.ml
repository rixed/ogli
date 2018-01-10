open Ogli
open Lr44

let debug = false

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

let clock =
  let seq = ref 0 in
  fun () ->
    incr seq ;
    !seq

module Param =
struct
  (* The non polymorphic part of a parameter: *)
  type desc =
    { name : string ; (* TODO: get rid of this *)
      mutable last_changed : int }

  (* A parameter is essentially a named ref cell.
   * Functions keep a reference to a param_desc param, so you cannot
   * delete params dynamically. *)
  type 'a t =
    { desc : desc ; mutable value : 'a ;
      mutable on_update : (unit -> unit) list }

  let make ?on_update name value =
    let on_update =
      match on_update with
      | None -> []
      | Some f -> [ f ]
    in
    { desc = { name ; last_changed = clock () } ; value ; on_update }

  let change p =
    List.iter (fun f -> f ()) p.on_update ;
    p.desc.last_changed <- clock ()

  let set p v =
    p.value <- v ;
    change p

  let on_update p f =
    assert (List.for_all (fun f' -> f != f') p.on_update) ;
    p.on_update <- f :: p.on_update

  (* For list params: *)
  let cons p v = set p (v :: p.value)

  (* For bool params: *)
  let toggle p = set p (not p.value)

  (* For int params: *)
  let incr p = set p (p.value + 1)
  let decr p = set p (p.value - 1)

  (* For optional params: *)
  let none p = option_may p.value (fun _ -> set p None)
end

type shape_tree = item Ogli_difftree.t
and item = Shape of Ogli_shape.t
         | Function of {
            (* We keep a reference to the param desc so we know when
             * the param is changed. *)
            param : Param.desc ;
            f : (unit -> shape_tree list) ;
            last_refresh : int }
         | NoHead

let fun_of p f =
  let head = Function {
    param = p.Param.desc ;
    f = (fun () -> f p.Param.value) ;
    last_refresh = 0 } in
  Ogli_difftree.make head []

let shape s l =
  let head = Shape s in
  Ogli_difftree.make head l

let group l =
  Ogli_difftree.make NoHead l

(* In painting order, aka from root to leaves: *)
let rec iter_shapes f tree =
  Ogli_difftree.iter_breadth_first (function
    | Function _ | NoHead -> () (* we assume the tree is up to date *)
    | Shape s -> f s) tree

(* Now a "window" must have essentially a difftree and a set of params: *)

type t =
  { mutable tree : shape_tree ;
    double_buffer : bool ;
    mutable past_tree : shape_tree option ; (* if double buffer *)
    min_coord : V.t ;
    max_coord : V.t ;
    width : int Param.t ;
    height : int Param.t ;
    mutable frame_num : int (* since last time our backbuffer was lost *) }

let make ?(min_coord = p 0. 0.) ?(max_coord = p 1. 1.)
         ~width ~height
         ?(double_buffer = false)
         tree =
  { tree ; double_buffer ; past_tree = None ;
    min_coord ; max_coord ; width ; height ;
    frame_num = 0 }

(* Drawing commands to update the canvas: *)
type cmd = Add of Ogli_shape.t | Del of Ogli_shape.t

let cmd_print fmt = function
  | Add s -> Format.fprintf fmt "Add %a" Ogli_shape.print s
  | Del s -> Format.fprintf fmt "Del %a" Ogli_shape.print s

let delete t s =
  let open Ogli_shape in
  if debug then Format.printf "delete bbox %a at pos %a@."
    Bbox.print s.bbox Point.print s.position ;
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
  | Function _ | NoHead -> cmd
  | Shape s -> Add s :: cmd

let del item cmd =
  match item with
  | Function _ | NoHead -> cmd
  | Shape s -> Del s :: cmd

(* Last children of a tree node get painted last (painter algorithm) *)
let render t =
  (* update the functions children whenever their param have changed: *)
  let next_tree = Ogli_difftree.map (function
    | Function { param ; f ; last_refresh }, key
      when last_refresh < param.last_changed ->
        if debug then Format.printf "Param %s has changed (%d) since last refresh (%d)@."
          param.name param.last_changed last_refresh ;
        let new_head =
          Some (Function { param ; f ; last_refresh = clock () }, key) in
        Some (new_head, f ())
    | _ -> None) t.tree in
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
  if debug then Format.printf "Commands for frame %d: %a@."
    t.frame_num (list_print cmd_print) cmds ;
  t.tree <- next_tree ;
  render_cmds t cmds ;
  t.frame_num <- t.frame_num + 1

(* Last children of a tree node receive the event (painter algorithm) *)
let next_event t get_event on_resize =
  let on_event event pos =
    if debug then Format.printf "Event %a at %a!@."
      print_event event Point.print pos ;
    try
      (* Like iter_depth_first, but with some additional exception
       * handlers for on_sub_click callbacks: *)
      let rec look_for_handler tree =
        List.iter loop (List.rev tree.Ogli_difftree.children) ;
        Lr44.option_may tree.head (function
          | (Shape s, _) ->
              let handler = Ogli_shape.handler_for_event s event in
              Lr44.option_may handler (fun f ->
                let bbox = Bbox.translate s.bbox s.position in
                if Bbox.is_inside bbox pos then (
                if debug then Format.printf "... Event is for bbox %a@." Bbox.print bbox ;
                f pos ;
                raise Exit))
          | _ -> ())
      and loop tree =
        match tree.head with
        | Some (Shape s, _) when event = Click && s.on_sub_click <> None ->
            (try look_for_handler tree
            with Exit -> (* click was handled *)
                Lr44.option_may s.on_sub_click (fun f -> f pos) ;
                raise Exit)
        | _ -> (* just recurse then *)
            look_for_handler tree
      in
      loop t.tree
    with Exit -> ()
  and on_remap w h =
    if debug then Format.printf "Remap event (w=%d, h=%d)@." w h ;
    t.frame_num <- 0 ; (* All back-buffers are lost *)
    Param.set t.width w ;
    Param.set t.height h ;
    on_resize w h
  in
  get_event ~on_event ~on_remap
