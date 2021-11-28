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


let fastExpMod x n m =
  let expBits = bitsFromInt n in
  let rec aux acc e = match e with
    | [] -> acc
    | h::t ->
      let accSqr = (acc * acc) mod m in
      let next = (accSqr * (if h == 1 then x else 1)) mod m in
      aux next t in
  aux 1 expBits

let _ = Printf.printf "%d\n" (fastExpMod 200023 72013 541) (*should output 121*)

