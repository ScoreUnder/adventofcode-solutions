open Batteries
open Printf

let navi_instructions = File.lines_of "input" |> map (fun line ->
                                                        let instr = line.[0] and mag = int_of_string (String.lchop line) in
                                                          (match instr with
                                                            |'L'|'R' -> assert ((mag mod 90) = 0)
                                                            | _-> ());
                                                          instr, mag
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

let coord_rotate x y times =
  let times = times +% 4 in
    match times with
      | 0 -> x, y
      | 1 -> -y, x
      | 2 -> -x, -y
      | 3 -> y, -x
      | _ -> assert false

let rec navigate_p2 x y (wx, wy) = function
  | ('N', d) :: tl -> navigate_p2 x y (wx, (wy - d)) tl
  | ('S', d) :: tl -> navigate_p2 x y (wx, (wy + d)) tl
  | ('E', d) :: tl -> navigate_p2 x y ((wx + d), wy) tl
  | ('W', d) :: tl -> navigate_p2 x y ((wx - d), wy) tl
  | ('L', d) :: tl -> navigate_p2 x y (coord_rotate wx wy (-d / 90)) tl
  | ('R', d) :: tl -> navigate_p2 x y (coord_rotate wx wy (d / 90)) tl
  | ('F', d) :: tl -> navigate_p2 (x + d * wx) (y + d * wy) (wx, wy) tl
  | [] -> x, y
  | _ -> assert false

let part1 () =
  manhattan_abs (navigate 0 0 0 navi_instructions)

let part2 () =
  manhattan_abs (navigate_p2 0 0 (10, -1) navi_instructions)

let () =
  printf "Part 1: %d\n%!" (part1 ());
  printf "Part 2: %d\n%!" (part2 ())
