let rec createRow = fun a n  ->
  if n = 0 then a ^ "\n" else createRow ("*" ^ a) (n-1)

let rec createGrid = fun a n m ->
  if m = 0 then a ^ "\n" else createGrid ((createRow "" n) ^ a) n (m-1)

let _ =
  let (x,y) = Scanf.scanf "%d %d" (fun a b -> (a, b)) in
  let _ = Printf.printf "%s\n" (createGrid "" y x) in
  ()
