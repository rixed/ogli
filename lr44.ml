let option_may x f =
  match x with None -> () | Some y -> f y
