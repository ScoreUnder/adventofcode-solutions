open Batteries
open Batteries.Enum
open Printf

let start_time, buses =
  let ch = open_in "input" in
  let start_time = int_of_string (input_line ch) in
  let buses =
    input_line ch |> String.split_on_char ','
    |> List.map (function "x" -> -1 | i -> int_of_string i)
  in
  close_in ch;
  (start_time, buses)

let buses_after time =
  buses |> List.enum
  |> filter (function -1 -> false | _ -> true)
  |> map (fun x -> (x, if time mod x = 0 then time else ((time / x) + 1) * x))

let earliest_bus_after time =
  buses_after time |> arg_min (fun (_, t) -> t)

let part1 () =
  let (id, time) = earliest_bus_after start_time in
    id * (time - start_time)

let () = printf "Part 1: %d\n%!" (part1 ())
