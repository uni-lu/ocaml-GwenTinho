let fstL l = match l with
  | [] -> 0
  | h::t -> h

let min l =
  let rec aux v = function
    | [] -> v
    | h::t -> if h < v then (aux h t) else (aux v t) in
    aux (fstL l) l

let _ = Printf.printf "%d \n" (min [1;3;-1;4; -5])
