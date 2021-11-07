(*not gonna pretend i came up with any of this, its just a slightly modified version of this article https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/modules/ex_queues.html*)

type 'a queue = 'a list * 'a list

let empty = ([],[])

let reverseList =
  let rec aux (l1: int list) (l2: int list) = match l2 with
    | [] -> l1
    | h::t -> aux (h::l1) t in
    aux []


let isEmpty = function
  | ([],[]) -> true
  | _ -> false

let norm = function
  | ([], back) -> (reverseList back, [])
  | q -> q

let enqueue q x =
  let f,b = q in
  norm (f, x::b)

let dequeue = function
  | ([],_) -> None
  | (_::xs, b) -> Some (norm (xs, b))

let peek = function
    | ([], _) -> None
    | (x::_, _) -> Some x

let rec iterQ f q =
  match peek q with
  | None -> ()
  | Some h -> let _ = f h in
  match dequeue q with
  | None -> ()
  | Some q -> iterQ f q

let mapQ f q =
  let rec aux f q acc =
  match peek q with
  | None -> acc
  | Some h -> match dequeue q with
    | None -> acc
    | Some q -> aux f q (enqueue acc (f h)) in
  aux f q empty

let rec printQ = iterQ (Printf.printf "%d \n")

let enqueueLst l =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (enqueue acc h) t in
  aux empty l

let _ = printQ (mapQ (fun x -> 2 * x) (enqueueLst [1;3;5;6]))
