open Ogli_geom

let () =
  let view = Ogli_view.make () in
  let flower =
    Ogli_compose.flower ~height:500. ~nb_leaves:5 ~base:(p 300. 30.) in
  let tree = Ogli_difftree.make (Ogli_view.Shape flower) [] in
  Ogli_view.render view tree
