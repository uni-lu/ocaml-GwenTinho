let rec expNaive x n = if n = 0 then 1 else x * expNaive x (n-1)

let rec expTemp x n a = if n = 0 then a else expTemp x (n-1) (a * x)

let expTR x n = expTemp x n 1


let _ = Printf.printf "%d\n" (expTR 10 5)
let _ = Printf.printf "%d\n" (expNaive 10 5)
