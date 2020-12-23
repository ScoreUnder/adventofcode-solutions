open Batteries
open Printf

let navi_instructions = File.lines_of "input" |> map (fun line ->
                                                        line.[0], int_of_string (String.lchop line)
) |> List.of_enum

let (+%) x y = (* positive modulo *)
  ((x mod y) + y) mod y

let directions = [|[|1; 0|]; [|0; 1|]; [|-1; 0|]; [|0; -1|]|]

let manhattan_abs (x, y) =
  (abs x) + (abs y)

let rec navigate x y dir = function
  | ('N', d) :: tl -> navigate x (y - d) dir tl
  | ('S', d) :: tl -> navigate x (y + d) dir tl
  | ('E', d) :: tl -> navigate (x + d) y dir tl
  | ('W', d) :: tl -> navigate (x - d) y dir tl
  | ('L', d) :: tl -> assert (d mod 90 = 0); navigate x y ((dir - (d / 90)) +% 4) tl
  | ('R', d) :: tl -> assert (d mod 90 = 0); navigate x y ((dir + (d / 90)) +% 4) tl
  | ('F', d) :: tl ->
      let vec = directions.(dir) in
      navigate (x + d * vec.(0)) (y + d * vec.(1)) dir tl
  | [] -> x, y
  | _ -> assert false

let part1 () =
  manhattan_abs (navigate 0 0 0 navi_instructions)

let () = printf "Part 1: %d\n%!" (part1 ())
