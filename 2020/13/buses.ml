open Batteries
open Batteries.Enum
open Printf

let start_time, buses =
  let ch = open_in "input" in
  let start_time = int_of_string (input_line ch) in
  let buses =
    input_line ch |> String.split_on_char ',' |> List.enum
    |> map (function "x" -> -1 | i -> int_of_string i)
    |> Array.of_enum
  in
  close_in ch;
  (start_time, buses)

let buses_after time =
  buses |> Array.enum
  |> filter (function -1 -> false | _ -> true)
  |> map (fun x -> (x, if time mod x = 0 then time else ((time / x) + 1) * x))

let earliest_bus_after time = buses_after time |> arg_min (fun (_, t) -> t)

let part1 () =
  let id, time = earliest_bus_after start_time in
  id * (time - start_time)

(* Part 2:
 * buses = bus(0) ... bus(n)
 * there exists k(0)...k(n) such that
 * bus(0)*k(0) - 0 = bus(1)*k(1) - 1 = ... = bus(n)*k(n) - n
 * find bus(0)*k(0).
 *
 * Normally I would want to brute-force this, by finding k for one of the
 * values.  It would be most efficient to start this search from the largest
 * bus(x) value.
 *
 * However brute force is inappropriate here due to the size of the
 * coefficient. It would take several hours. So I need some mathematical brain
 * power here...
 * *)
let compact_buses buses =
  buses |> Array.enum
  |> mapi (fun i bus -> (i, bus))
  |> filter (fun (_, bus) -> bus <> -1)
  |> Array.of_enum

let ind_of_largest_bus compact_buses =
  let ind, _ = compact_buses |> Array.enum |> arg_max (fun (_, bus) -> bus) in
  ind

let for_alli f arr =
  let rec aux i =
    if i >= Array.length arr then true
    else if f i arr.(i) then aux (i + 1)
    else false
  in
  aux 0

let get_period start f =
  let open Z in
  let rec aux i = if f i then i else aux (succ i) in
  let first = aux start in
  let second = aux (succ first) in
  let period = second - first in
  if f (period + second) then (first, period) else assert false

let part2 () =
  let open Z in
  let cbuses = compact_buses buses in
  let largest_bus = ind_of_largest_bus cbuses in
  let rec acc_periods start period i =
    if to_int i >= Array.length cbuses then (start, period)
    else
      let start2, period2 =
        get_period start (fun k ->
            let time =
              (~$(buses.(largest_bus)) * ((k * period) + start)) - ~$largest_bus
            in
            let bi, bus = cbuses.(to_int i) in
            (~$bi + time) mod ~$bus = zero)
      in
      acc_periods (start + (start2 * period)) (period * period2) (succ i)
  in
  let start, period = acc_periods one one zero in
  let fullperiod =
    Array.fold_left (fun acc (_, bus) -> lcm acc ~$bus) one cbuses
  in
  let time = (~$(buses.(largest_bus)) * start) - ~$largest_bus in
  printf "start = %s; period = %s; fullperiod = %s; t = %s\n%!"
    (to_string start) (to_string period) (to_string fullperiod) (to_string time);
  time mod fullperiod

let () =
  printf "Part 1: %d\n%!" (part1 ());
  printf "Part 2: %s\n%!" (Z.to_string (part2 ()))
