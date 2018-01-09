(* A differentiable tree is a tree which items order are not maintained by the
 * tree (they will be reordered to ease diffing) and that offer a simple diff
 * method to update something based on its past and next representation as a
 * tree. *)

(* Many time the order of children will stay the same, as the tree will
 * always be constructed in the same way (using empty groups that still
 * occupy a slot) for optional elements. But sometime we will want a key
 * to identity who should be diffed with who.
 *)

(*$inject
  open Batteries
  type op = Add of int | Del of int
  let e = empty []
  let add x ctx = Add x :: ctx
  let del x ctx = Del x :: ctx
  let diff = diff del add
*)

type key_kind = KI of int | KS of string
type key = key_kind list
let no_key = []

type 'a t =
  { mutable head : ('a * key) option ;
    mutable children : 'a t list (* ordered by construction *) }

let make ?(key=[]) v children =
  { head = Some (v, key) ; children }

let empty children =
  { head = None ; children }

let extract_key k ts =
  let rec loop prev = function
    | [] ->
        None
    | { head = Some (_, k') ; _ } as t :: ts when k = k' ->
        Some (t, List.rev_append prev ts)
    | t :: ts ->
        loop (t :: prev) ts in
  loop [] ts

(*$= extract_key & ~printer:dump
  None \
    (extract_key [KI 42] [ { head = None ; children = [] } ; \
                           { head = Some (1, [KI 1]) ; children = [] } ])
  (Some ({ head = Some (1, [KI 42]) ; children = [] }, \
         [ { head = None ; children = [] } ; \
           { head = Some (2, [KI 5]) ; children = [] } ])) \
    (extract_key [KI 42] [ { head = None ; children = [] } ; \
                           { head = Some (1, [KI 42]) ; children = [] } ;\
                           { head = Some (2, [KI 5]) ; children = [] } ])
 *)

(* [add] and [del] are given the context and a 'a and return the new context. *)
let rec diff_children del add befs afts ctx =
  let addrec ctx t =
    let ctx = match t.head with None -> ctx | Some (v, _) -> add v ctx in
    diff_children del add [] t.children ctx
  and delrec ctx t =
    let ctx = match t.head with None -> ctx | Some (v, _) -> del v ctx in
    diff_children del add t.children [] ctx in
  let rec loop ctx = function
    | [], [] -> ctx
    | [], aft :: afts ->
        let ctx = addrec ctx aft in
        loop ctx ([], afts)
    | bef :: befs, [] ->
        let ctx = delrec ctx bef in
        loop ctx (befs, [])
    (* Check the keys before to resorting to position *)
    | ({ head = Some (vbef, (_::_ as kbef)) ; _ } as bef :: befs'), afts ->
        (* before is keyed: *)
        (match extract_key kbef afts with
        | None ->
          (* No such key, just del this item recursively. *)
          let ctx = delrec ctx bef in
          loop ctx (befs', afts)
        | Some (aft, afts') ->
          let ctx = diff del add bef aft ctx in
          loop ctx (befs', afts'))
    | befs, ({ head = Some (vaft, (_::_ as kaft)) ; _ } as aft :: afts') ->
        (* after is keyed: *)
        (match extract_key kaft befs with
        | None ->
          (* No such key just add this item recursively. *)
          let ctx = addrec ctx aft in
          loop ctx (befs, afts')
        | Some (bef, befs') ->
          let ctx = diff del add bef aft ctx in
          loop ctx (befs', afts'))
    | bef :: befs', aft :: afts' ->
      (* Anything else is managed by diff. *)
      let ctx = diff del add bef aft ctx in
      loop ctx (befs', afts')
  in
  loop ctx (befs, afts)

(* Update the head and diff the children: *)
and diff del add bef aft ctx =
  let ctx =
    match bef.head, aft.head with
    | None, None -> ctx
    | None, Some (aft, _) -> add aft ctx
    | Some (bef, _), None -> del bef ctx
    | Some (bef, _), Some (aft, _) ->
        (* With == we are going to redraw a lot of identical stuff (each
         * time we refresh a function none of the items in its subtree will
         * be == to the previous instance, but many may be =.
         * TODO: provide an equality operator with the tree? *)
        if aft == bef then ctx
        else (del bef ctx |> add aft) in
  diff_children del add bef.children aft.children ctx

(*$= diff & ~printer:dump
  [] \
    (diff e e [])
  [Add 1] \
    (diff e (make 1 []) [])
  [Add 2; Add 1] \
    (diff e (make 1 [ make 2 [] ]) [])
  [Add 3; Add 2; Add 1] \
    (diff e (make 1 [ make 2 [] ; make 3 [] ]) [])
  [Add 3; Add 2; Add 1] \
    (diff e (make 1 [ make 2 [ make 3 [] ] ]) [])

  [Del 1] \
    (diff (make 1 []) e [])
  [Del 2; Del 1] \
    (diff (make 1 [ make 2 [] ]) e [])
  [Del 3; Del 2; Del 1] \
    (diff (make 1 [ make 2 [] ; make 3 [] ]) e [])
  [Del 3; Del 2; Del 1] \
    (diff (make 1 [ make 2 [ make 3 [] ] ]) e [])

  [] \
    (diff (make 1 []) \
          (make 1 []) [])
  [] \
    (diff (make 1 [ make 2 [] ]) \
          (make 1 [ make 2 [] ]) [])
  [] \
    (diff (make 1 [ make 2 [] ; make 3 [] ]) \
          (make 1 [ make 2 [] ; make 3 [] ]) [])

  [Add 2; Del 1] \
    (diff (make 1 []) (make 2 []) [])
  [Add 4; Del 2] \
    (diff (make 1 [ make 2 [] ; make 3 [] ]) \
          (make 1 [ make 4 [] ; make 3 [] ]) [])
  [Del 3] \
    (diff (make 1 [ make 2 [] ; make 3 [] ]) \
          (make 1 [ make 2 [] ]) [])
  [Del 2] \
    (diff (make 1 [ make 2 [] ; make 3 [] ]) \
          (make 1 [ empty []  ; make 3 [] ]) [])
  [] \
    (diff (make 1 [ make ~key:[KI 1] 2 [] ; make ~key:[KI 2] 3 [] ]) \
          (make 1 [ make ~key:[KI 2] 3 [] ; make ~key:[KI 1] 2 [] ]) [])
  [Del 2] \
    (diff (make 1 [ make ~key:[KI 1] 2 [] ; make ~key:[KI 2] 3 [] ]) \
          (make 1 [ make ~key:[KI 2] 3 [] ]) [])
  [Add 4; Del 2] \
    (diff (make 1 [ make ~key:[KI 1] 2 [] ; make ~key:[KI 2] 3 [] ]) \
          (make 1 [ make 4 [] ; make ~key:[KI 2] 3 [] ]) [])
 *)

(* Useful to update the tree: a function that crawl the tree (depth first)
 * and builds a new one, persisting what can be: *)
let rec map f t =
  let head, children =
    match t.head with
    | None -> t.head, t.children
    | Some hd ->
        (match f hd with
        | None -> t.head, t.children
        | Some x -> x) in
  { head ;
    children = List.map (map f) children }

let rec iter_depth_first f t =
  List.iter (iter_depth_first f) t.children ;
  Lr44.option_may t.head (fun (s, _) -> f s)

let rec iter_breadth_first f t =
  Lr44.option_may t.head (fun (s, _) -> f s) ;
  List.iter (iter_breadth_first f) t.children
