open Printf

let play_game initial max_turns =
  let acc = Array.init max_turns (fun _ -> -1) in
  let max_turns = pred max_turns in
  let rec play_turn tn next =
    if tn = max_turns then next
    else
      let prev_tn = acc.(next) in
      let mine = if prev_tn = (-1) then 0 else tn - prev_tn in
      acc.(next) <- tn;
      play_turn (succ tn) mine
  in
  initial |> List.iteri (fun i v -> acc.(v) <- i);
  play_turn (List.length initial) 0

let () =
  printf "Part 1: %d\n%!" (play_game [ 8; 11; 0; 19; 1; 2 ] 2020);
  printf "Part 2: %d\n%!" (play_game [ 8; 11; 0; 19; 1; 2 ] 30000000)
