let min l = match l with
  | [] -> 0
  | h::t ->
  let rec aux m l = match l with
    | [] -> m
    | h::t -> if h < m then aux h t else aux m t in
  aux h t

let reverseList =
  let rec aux l1 l2 = match l2 with
    | [] -> l1
    | h::t -> aux (h::l1) t in
    aux []

let rec splitFilter f l =
  let rec aux l accPos accNeg = match l with
  | [] -> (accPos, accNeg)
  | h::t -> if f h then aux t (h::accPos) accNeg else aux t accPos (h::accNeg) in
  let pos, neg = (aux l [] []) in
  (reverseList pos, reverseList neg)

let rec prependList l1 l2 =
  let rec aux l1 acc = match l1 with
    | [] -> acc
    | h::t -> aux t (h::acc) in
  aux l1 l2

let selectionSort l =
  let rec aux l acc = match l with
    | [] -> acc
    | l ->
      let mv = min l in
      let pos, neg = splitFilter (fun x-> x==mv) l in
      aux neg (prependList pos acc) in
  reverseList (aux l [])

let _ = List.iter (Printf.printf "%d \n") (selectionSort [4;0;-1;-2;3])
