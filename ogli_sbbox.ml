(* We want to manipulate bounding boxes that are efficient to process yet
 * easy to compute.
 * So we will have a special bbox, called a smart-bbox, aka sbbox, that is
 * a supperposition of normal bboxes. The idea is to combine them when and
 * only when it does not save much space to keep them separated. *)
open Ogli

type t =
  { mutable bboxes : Bbox.t list }

let empty = { bboxes = [] }

let is_empty sb = sb.bboxes = []

let singleton b = { bboxes = [ b ] }

let add sb b =
  if is_empty sb then singleton b else
  (* Simple implementation of all: merge everything, all the time! *)
  { bboxes = [ Bbox.union (List.hd sb.bboxes) b ] }

let merge sb sb' =
  if is_empty sb then sb' else
  if is_empty sb' then sb else
  List.fold_left add sb sb'.bboxes

let translate v sb =
  { bboxes = List.map (fun b -> Bbox.translate b v) sb.bboxes }
