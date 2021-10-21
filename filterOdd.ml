let rec filterOdd = function
  | [] -> []
  | x::l -> if (x mod 2 = 1) then x::(filterOdd l) else (filterOdd l)
let printList l = List.iter (Printf.printf "%d ") l

let _ = printList (filterOdd [0;1;4;5])
