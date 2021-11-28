let rec expNaive x n = if n = 0 then 1 else x * expNaive x (n-1)

let rec expTemp x n a = if n = 0 then a else expTemp x (n-1) (a * x)

let expTR x n =
  let rec aux a = function
    | 0 -> a
    | m -> aux (a * x) (m - 1) in
  aux 1 n



(*note that this algo gladly ignores negative values :)*)
(*creates list of bits with most significant bit at the front*)
let bitsFromInt x =
  let rec aux l x = match x with
    | 0 -> l
    | x ->
      let lowest = x mod 2 in
      aux (lowest::l) (x/2) in
  aux [] x

let fastExp x n =
  let expBits = bitsFromInt n in
  let rec aux acc e = match e with
    | [] -> acc
    | h::t ->
      let accSqr = acc * acc in
      let next = accSqr * (if h == 1 then x else 1) in
      aux next t in
  aux 1 expBits

(*uses montgomery ladder apparently*)
let fastExpMod x n m =
  let expBits = bitsFromInt n in
  let rec aux acc e = match e with
    | [] -> acc
    | h::t ->
      let accSqr = (acc * acc) mod m in
      let next = (accSqr * (if h == 1 then x else 1)) mod m in
      aux next t in
  aux 1 expBits


let _ = Printf.printf "%d\n" (fastExpMod 200023 272113 541) (*should output 423*)

(*the following is not tail recursive, but doesnt use that call to bits from int*)

let dumbExpMod x n m =
  let rec aux n = match n with
    | 0 -> 1
    | n ->
      let value = (aux (n/2)) mod m in
      let sqr = (value * value) mod m in
      match n mod 2 with
        | 0 ->  sqr
        | 1 -> (sqr * x) mod m
        | _ -> -1 in
  aux n

let _ = Printf.printf "%d\n" (dumbExpMod 200023 272113 541) (*should output 423*)


(*https://www.youtube.com/watch?v=WAzGvZbaAOw*)
(*the best implementation since it doesnt need a binary conversion + it uses tail recursion*)
let smortExpMod x n m =
  let rec aux acc x = function
    | 0 -> acc
    | n ->
      let next = if n mod 2 == 0 then acc else (acc * x) mod m in
      aux next ((x * x) mod m) (n/2) in
  aux 1 x n

let _ = Printf.printf "%d\n" (smortExpMod 200023 272113 541) (*should output 423*)
