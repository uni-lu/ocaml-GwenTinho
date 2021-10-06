let rec desc = fun n ->
  let y = n - 1 in
  let _ = Printf.printf "%d\n" y in
  if y = 0 then 0 else desc y


let rec asc = fun n a ->
  let y = a - 1 in
  let _ = Printf.printf "%d\n" (n - y) in
  if y = 0 then 0 else asc n y

  (*Get asc to work correctly*)
let _ =
  let x = Scanf.scanf "%d" (fun x -> x) in
  desc x;
  asc x x
