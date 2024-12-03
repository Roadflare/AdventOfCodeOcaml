let parse filename =
  let channel = open_in filename in
  let rec read_lines acc1 acc2 =
    try
      let (a1, a2) = Scanf.sscanf (input_line channel) "%d %d" (fun x y -> x,y) in
      read_lines (a1 :: acc1) (a2 :: acc2)

    with End_of_file ->
      close_in channel;
      (acc1, acc2)
    in
    read_lines [] [] (* Input reading sem dobil iz https://github.com/supermii2/aoc2024/blob/main/day1/day1.ml
    ostalo je moje delo *)

let day1_1 = 
  let (x, y) = parse "input.1" in
  let sortedx = List.sort compare x in
  let sortedy = List.sort compare y in
  let res = List.fold_left ( + ) 0 (List.map2 (fun x y -> abs (x - y)) sortedx sortedy) in
  Printf.printf "Result day1_1: %d \n" res

let count_occur el list =
  let rec aux res ele lst =
  match lst with
  | [] -> res
  | x :: xs -> if x = el then aux (res + 1) ele xs else aux res ele xs
in aux 0 el list
let day1_2 =
  let (x, y) = parse "input.1" in
  let sortedx = List.sort_uniq compare x in
  let sortedy = List.sort compare y in
  let rec aux l1 l2 i =
    match l1 with
    | [] -> Printf.printf "Result day1_2: %d \n" i
    | x :: xs ->
      let count = count_occur x l2 in
    aux xs l2 (i + (x * count))
    in aux sortedx sortedy 0
