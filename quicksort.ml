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

let partition l = match l with
  | [] -> (([],[]),0)
  | h::t -> (splitFilter (fun x -> x < h) t, h)

let rec quickSort = function
    | [] -> []
    | h::[] -> [h]
    | l ->
      let split, pivot = partition l in
      let lo,hi = split in
      prependList (reverseList (quickSort lo)) (pivot::(quickSort hi))


let _ = List.iter (Printf.printf "%d \n") (quickSort [7;1;2;12;-2;3;-1;4])
