let addOne l =List.map (fun a -> a + 1) l

let printList l = List.iter (Printf.printf "%d ") l

let _ = printList (addOne [0;1])
