let option_may f x =
  match x with None -> () | Some y -> f y

let option_map f x =
  match x with None -> None | Some y -> Some (f y)

let list_init n f =
  let rec loop prev i =
    if i >= n then List.rev prev else loop (f i :: prev) (i + 1) in
  loop [] 0

let iter_print iter printer fmt lst =
  Format.fprintf fmt "@[[" ;
  iter (fun i ->
    Format.fprintf fmt "%a@;" printer i) lst ;
  Format.fprintf fmt "]@]"

let list_print p = iter_print List.iter p
let array_print p = iter_print Array.iter p

let pos_mod a b =
  let x = a mod b in
  if x >= 0 then x else x + b
