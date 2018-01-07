let option_may x f =
  match x with None -> () | Some y -> f y

let list_print printer fmt lst =
  Format.fprintf fmt "@[[" ;
  List.iter (fun i ->
    Format.fprintf fmt "%a@;" printer i) lst ;
  Format.fprintf fmt "]@]"
