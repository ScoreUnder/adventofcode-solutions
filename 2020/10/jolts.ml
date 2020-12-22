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
        in make_enum (ref x) e
  | None -> Enum.empty ()

let part1 () =
  let diffs = adapters |> Set.add 0 |> Set.add (Set.max_elt adapters + 3) |> Set.enum |> map_pairs (-) |> List.of_backwards in
  (List.count_matching ((=) (-1)) diffs) * (List.count_matching ((=) (-3)) diffs)

let () = printf "Part 1: %d\n%!" (part1 ())
