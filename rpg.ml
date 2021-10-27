Random.self_init ()

type charClass =  Ranger | Warrior | Magician
type charObject = GoldCoins | ChickenWings | Sponges
type charBag = (charObject * int) list
type race = Golem | Boar | MosquitoesSwarm of int

exception Death of string

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
  | 2 -> MosquitoesSwarm (Random.int 10)
  | _ -> Golem (* shouldnt be able to happen*)

let hit (c:character) =
  let damage, chance = match c.cls with
    | Warrior -> (10, 0.3)
    | Ranger -> (4, 0.7)
    | Magician -> (5,0.5) in
  let roll = Random.float 1.0 in
    if roll < (chance +. 0.05 *. (float_of_int c.xp)) then damage else 0

let monster_hit (m: monster) =
  let r = m.rc in
  match r with
    | Golem -> 4
    | MosquitoesSwarm n ->  n / 2 (*no idea how to make it swarm*)
    | Boar -> 2

let fight (c: character) (m: monster) =
  let battleOne = m.hp - (hit c) in
  if battleOne <= 0 then {c with xp = c.xp + 5}
  else let battleTwo = c.hp - (monster_hit m) in
  if battleTwo <= 0 then raise (Death "The monster killed you") else {c with hp = battleTwo; xp = c.xp + 5}

let unfortunate_encounter (c: character) =
  let m = random_monster () in
  m (*finish this next time*)
