open Batteries
open Printf

let adapters = File.lines_of "input" |> map int_of_string |> Set.of_enum

let map_pairs f e =
  match Enum.get e with
  | Some x ->
      let rec make_enum last e =
        Enum.make
          ~next:(fun () ->
            let next = Enum.get_exn e in
            let result = f !last next in
            last := next;
            result)
          ~count:(fun () -> Enum.count e)
          ~clone:(fun () -> make_enum (ref !last) (Enum.clone e))
      in
      make_enum (ref x) e
  | None -> Enum.empty ()

let adapters_ex = adapters |> Set.add 0 |> Set.add (Set.max_elt adapters + 3)

let part1 () =
  let diffs = adapters_ex |> Set.enum |> map_pairs ( - ) |> List.of_backwards in
  List.count_matching (( = ) (-1)) diffs
  * List.count_matching (( = ) (-3)) diffs

let memoize f =
  let tbl = Hashtbl.create (Set.cardinal adapters_ex) in
  fun v ->
    match Hashtbl.find_option tbl v with
    | Some res -> res
    | None ->
        let res = f v in
        Hashtbl.add tbl v res;
        res

let memoize_rec f_norec =
  let f = ref (fun _ -> assert false) in
  let memoized = memoize (fun v -> f_norec !f v) in
  f := memoized;
  memoized

let count_adapter_combinations lst =
  let aux f lst =
    match lst with
    | [ a; b ] when b - a <= 3 -> 1
    | a :: b :: tl when b - a <= 3 -> f (b :: tl) + f (a :: tl)
    | _ -> 0
  in
  (memoize_rec aux) lst

let part2 () = count_adapter_combinations (Set.to_list adapters_ex)

let () =
  printf "Part 1: %d\n%!" (part1 ());
  printf "Part 2: %d\n%!" (part2 ())
