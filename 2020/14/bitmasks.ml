open Batteries
open Printf

type instr = Mask of int64 * int64 | Mem of int64 * int64

let int64_of_bin bin =
  let len = String.length bin in
  let rec aux acc i =
    if i = len then acc
    else
      match bin.[i] with
      | '0' -> aux (Int64.shift_left acc 1) (succ i)
      | '1' -> aux (Int64.logor (Int64.shift_left acc 1) 1L) (succ i)
      | _ -> failwith "not binary"
  in
  aux 0L 0

let input =
  File.lines_of "input"
  |> map (fun line ->
         if String.starts_with line "mask = " then
           let xstr = String.lchop line ~n:7 in
           let and_mask =
             int64_of_bin (String.nreplace ~str:xstr ~sub:"X" ~by:"1")
           in
           let or_mask =
             int64_of_bin (String.nreplace ~str:xstr ~sub:"X" ~by:"0")
           in
           Mask (and_mask, or_mask)
         else if String.starts_with line "mem[" then
           let lbr = String.find line "[" + 1 in
           let rbr = String.find line "]" - lbr in
           let eq = String.find line " = " + 3 in
           Mem
             ( Int64.of_string (String.sub line lbr rbr),
               Int64.of_string (String.lchop line ~n:eq) )
         else failwith "bad instruction")
  |> List.of_enum

let run_program () =
  let rec run_one mem andm orm = function
    | Mask (andm, orm) :: tl -> run_one mem andm orm tl
    | Mem (addr, value) :: tl ->
        run_one
          (Map.add addr (Int64.logor (Int64.logand value andm) orm) mem)
          andm orm tl
    | [] -> mem
  in
  run_one Map.empty 0xFFFFFFFFFL 0L input

let part1 () = run_program () |> Map.values |> Enum.reduce Int64.add

let () = printf "Part 1: %Ld\n%!" (part1 ())
