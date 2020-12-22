open Batteries
open Printf

let nums = File.lines_of "input" |> map int_of_string |> List.of_enum

let rec find_with_depth depth f = function
  | _ when depth = 0 -> None
  | hd :: tl when f (depth - 1) hd tl -> Some hd
  | _ :: tl -> find_with_depth (depth - 1) f tl
  | [] -> None

let invalid_xmas preamble num =
  preamble
  |> find_with_depth 25 (fun depth n1 ->
         find_with_depth depth (fun _ n2 _ -> n1 + n2 = num) %> Option.is_some)
  |> Option.is_none

let part1 () =
  let rec aux nums remaining =
    match remaining with
    | hd :: _ when invalid_xmas nums hd -> hd
    | hd :: tl -> aux (hd :: nums) tl
    | [] -> assert false
  in
  let initial, remaining = nums |> List.split_at 25 in
  aux initial remaining

let () = printf "Part 1: bad number is %d\n%!" (part1 ())
