open Printf

let play_game initial max_turns =
  let acc = Hashtbl.create 4000 in
  let max_turns = pred max_turns in
  let rec play_turn tn next =
    if tn = max_turns then next
    else
      let mine = tn - BatHashtbl.find_default acc next tn in
      Hashtbl.replace acc next tn;
      play_turn (succ tn) mine
  in
  initial |> List.iteri (fun i v -> Hashtbl.replace acc v i);
  play_turn (List.length initial) 0

let () =
  printf "Part 1: %d\n%!" (play_game [ 8; 11; 0; 19; 1; 2 ] 2020);
  printf "Part 2: %d\n%!" (play_game [ 8; 11; 0; 19; 1; 2 ] 30000000)
