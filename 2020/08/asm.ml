open Batteries
open Printf
open Re

type instruction = Acc of int | Jmp of int | Nop of int

let instruction_regex =
  {|(acc|nop|jmp) ([+-]?\d+)|}
  |> Perl.re ~opts:[ `Anchored; `Caseless ]
  |> compile

let instruction_of_string s =
  try
    Some
      ((exec instruction_regex s |> Group.all |> function
        | [| _; instr; num |] -> (String.lowercase instr, int_of_string num)
        | _ -> failwith "impossible")
       |> function
       | "acc", num -> Acc num
       | "jmp", num -> Jmp num
       | "nop", num -> Nop num
       | _ -> failwith "impossible")
  with Not_found -> None

let program =
  File.lines_of "input" |> filter_map instruction_of_string |> Vect.of_enum

(* Run the program until either infinite loop or natural end.
   Returns tuple of acc * finished_normally
*)
let run_program program =
  let visited_instrs = BitSet.create (Vect.length program) in
  let rec exec_one pc acc =
    if BitSet.mem visited_instrs pc then
      (* If we already visited this instruction, stop. *)
      (acc, false)
    else if pc >= Vect.length program then
      (* Program has ended normally *)
      (acc, true)
    else (
      BitSet.set visited_instrs pc;
      match Vect.get program pc with
      | Acc n -> exec_one (pc + 1) (acc + n)
      | Jmp n -> exec_one (pc + n) acc
      | Nop _ -> exec_one (pc + 1) acc )
  in
  exec_one 0 0

let part1 () =
  let acc, finished = run_program program in
  assert (not finished);
  acc

let part2 () =
  Enum.range 0 ~until:(Vect.length program - 1)
  |> filter_map (fun i ->
         match Vect.get program i with
         | Acc _ -> None
         | Jmp x -> Some (Vect.set program i (Nop x))
         | Nop x -> Some (Vect.set program i (Jmp x)))
  |> filter_map (fun prog ->
         let acc, finished = run_program prog in
         if finished then Some acc else None)

let () =
  printf "Part 1: Stopped at instruction %d\n%!" (part1 ());
  part2 () |> iter (printf "Part 2: Finished at instruction %d\n%!")
