Random.self_init ()

type charClass =  Ranger | Warrior | Magician
type charObject = GoldCoins | ChickenWings | Sponges
type charBag = (charObject * int) list
type race = Golem | Boar | MosquitoesSwarm


type character = {
  cls: charClass;
  bag: charBag;
  hp: int;
  xp: int
}

type monster = {
  rc: race;
  hp: int;
  obj: charObject
}


let addObjectToBag (b: charBag) (o: charObject * int) = o::b

let print_bag (b: charBag) = List.iter (fun (o:  charObject * int) ->
  let obj, count = o in
  let objType = match obj with
  | GoldCoins -> "coins"
  | ChickenWings -> "chicken wing"
  | Sponges -> "Sponges" in
  Printf.printf "%d %s\n" count objType
) b

let random_monster () =
  let x = (Random.int 3) in
  match x with
  | 0 -> Golem
  | 1 -> Boar
  | 2 -> MosquitoesSwarm
  | _ -> Golem (* shouldnt be able to happen*)

let hit (c:character) = (*for next time*)
