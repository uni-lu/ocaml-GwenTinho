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

let appendFast l1 l2 =
  let rec aux l2 acc = match l2 with
    | [] -> acc
    | h::t -> aux t (h::acc) in
    reverseList (aux l2 (reverseList l1))

let partition l = match l with
  | [] -> (([],[]),0)
  | h::t -> (splitFilter (fun x -> x < h) t, h)

let rec quickSort = function
    | [] -> []
    | pivot::t ->
      let lo, hi = splitFilter (fun x -> x < pivot) t in
      appendFast (quickSort lo) (pivot::(quickSort hi))


let printl l =
  let _ = print_string (String.concat " " (List.map string_of_int l)) in
  Printf.printf "\n"

let _ = printl (quickSort [7;1;2;12;-2;3;-1;4])
