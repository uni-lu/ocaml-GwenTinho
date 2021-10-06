let x = Scanf.scanf "%d" (fun x -> x) in
let y = if x mod 2 = 0 then "even" else "odd" in
let _ = Printf.printf "%s\n" y in
()
