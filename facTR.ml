let rec facTemp = fun n a ->
  if n = 0 then a else facTemp (n-1) (a * n)

let facTR n= facTemp n 1


let rec facBad = fun n ->
  if n = 0 then 1 else n * (facBad (n-1))

let _ = Printf.printf "%d\n" (facTR 10)
let _ = Printf.printf "%d\n" (facBad 10)
