open Batteries
open Printf

let play_game initial max_turns =
  let initial_turns = initial |> List.fold_lefti (fun acc i v -> Map.add v i acc) Map.empty in
  let max_turns = pred max_turns in
  let rec play_turn acc tn next =
    if tn = max_turns then next else
    let mine = match Map.find_opt next acc with
      | Some prev_tn -> tn - prev_tn
      | None -> 0
    in
      play_turn (Map.add next tn acc) (succ tn) mine
  in
    play_turn initial_turns (List.length initial) 0

let () =
  printf "Part 1: %d\n%!" (play_game [8;11;0;19;1;2] 2020);
  printf "Part 2: %d\n%!" (play_game [8;11;0;19;1;2] 30000000)
