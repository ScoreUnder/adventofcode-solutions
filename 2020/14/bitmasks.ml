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

let run_program memset =
  let rec run_one mem andm orm = function
    | Mask (andm, orm) :: tl -> run_one mem andm orm tl
    | Mem (addr, value) :: tl ->
        run_one (memset addr value andm orm mem) andm orm tl
    | [] -> mem
  in
  run_one Map.empty 0xFFFFFFFFFL 0L input |> Map.values |> Enum.reduce Int64.add

let memset_p1 addr value andm orm mem =
  Map.add addr (Int64.logor (Int64.logand value andm) orm) mem

let part1 () = run_program memset_p1

let address_options andm orm addr =
  let addr = Int64.logor addr orm in
  let exes = Int64.logxor orm andm in
  let rec split_addrs a n =
    let nextn = Int64.shift_right n 1 in
    if n = 0L then [ a ]
    else if Int64.logand n exes = 0L then split_addrs a nextn
    else
      let a1 = Int64.logor a n in
      let a0 = Int64.logxor a1 n in
      List.rev_append (split_addrs a0 nextn) (split_addrs a1 nextn)
  in
  split_addrs addr (Int64.shift_left 1L 35)

let memset_p2 addr value andm orm mem =
  address_options andm orm addr
  |> List.fold_left (fun acc addr -> Map.add addr value acc) mem

let part2 () = run_program memset_p2

let () =
  printf "Part 1: %Ld\n%!" (part1 ());
  printf "Part 2: %Ld\n%!" (part2 ())
