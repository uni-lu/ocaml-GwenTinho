let rec fold_left2 res acc = function
  | [] -> res
  | h::t -> fold_left2 (acc res h) acc t



let printList l = List.iter (Printf.printf "%d ") l

let _ = Printf.printf "%d \n" (fold_left2 1 (fun x y -> x * y) [1;4;5])
