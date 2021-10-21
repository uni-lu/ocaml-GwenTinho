let rec reverseList (l1: int list) (l2: int list) = match l2 with
  | [] -> l1
  | h::t -> reverseList (h::t) l1


let printList l = List.iter (Printf.printf "%d ") l

let _ = printList (reverseList [] [0;1;4;5])
