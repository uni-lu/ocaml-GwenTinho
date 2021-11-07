let reverseList =
  let rec aux (l1: int list) (l2: int list) = match l2 with
    | [] -> l1
    | h::t -> aux (h::l1) t in
    aux []

(*very dumb implementation*)
let append l1 l2 =
  let rec aux l1 l2 acc = match l2 with
    | [] -> acc
    | h::t -> aux l1 t (reverseList (h::(reverseList acc))) in
    aux l1 l2 l1

let appendFast l1 l2 =
  let rec aux l2 acc = match l2 with
    | [] -> acc
    | h::t -> aux t (h::acc) in
    reverseList (aux l2 (reverseList l1))


let printList l = let _ = List.iter (Printf.printf "%d ") l in
  Printf.printf "\n"

let _ = printList (appendFast [0;1;4;5] [2;3;4;5;6])
