let rec iter f = function
  | [] -> []
  | h::t -> let _ = f h in
  iter f t

let _ = iter (Printf.printf "%d\n") [1;2;3]
