let rec filter f = function
  | [] -> []
  | x::l -> if f x then x::(filter f l) else (filter f l)
let printList l = List.iter (Printf.printf "%d ") l

let _ = printList (filter (fun x -> x mod 2 = 0) [0;1;4;5])
