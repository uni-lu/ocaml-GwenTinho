let reverseList =
  let rec aux (l1: int list) (l2: int list) = match l2 with
    | [] -> l1
    | h::t -> aux (h::l1) t in
    aux []

let append l x = reverseList (x::(reverseList l))

let printList l = let _ = List.iter (Printf.printf "%d ") l in
  Printf.printf "\n"

let _ = printList (append [0;1;4;5] 2)
