let option_may x f =
  match x with None -> () | Some y -> f y

let list_init n f =
  let rec loop prev i =
    if i >= n then List.rev prev else loop (f i :: prev) (i + 1) in
  loop [] 0

let list_print printer fmt lst =
  Format.fprintf fmt "@[[" ;
  List.iter (fun i ->
    Format.fprintf fmt "%a@;" printer i) lst ;
  Format.fprintf fmt "]@]"

let pos_mod a b =
  let x = a mod b in
  if x >= 0 then x else x + b
