open Batteries
open Printf

module Field2D = struct
  type 'a t = 'a Vect.t Vect.t

  let make e : 'a t =
    let res = e |> Enum.map Vect.of_enum |> Vect.of_enum in
    assert (
      Vect.for_all (fun el -> Vect.length el = Vect.length (Vect.get res 0)) res
    );
    res

  let get (fld : 'a t) x y = Vect.get (Vect.get fld y) x

  let get3x3 (fld : 'a t) x y =
    let ys = max 0 (y - 1) -- min (Vect.length fld - 1) (y + 1) in
    let xs = max 0 (x - 1) -- min (Vect.length (Vect.get fld 0) - 1) (x + 1) in
    ys
    |> Enum.concat_map (fun y ->
           let row = Vect.get fld y in
           xs |> Enum.clone |> Enum.map (Vect.get row))

  let mapi f (fld: 'a t): 'a t =
    fld |> Vect.mapi (fun y row -> 
      row |> Vect.mapi (fun x el -> f x y el))

  let show (fld: char t) =
    String.concat "\n" (fld |> Vect.map (Vect.enum %> String.of_enum) |> Vect.to_list)

  let count_if f (fld: 'a t) =
    fld |> Vect.enum |> Enum.map (Vect.enum %> Enum.filter f %> Enum.hard_count) |> Enum.sum
end

let initial = File.lines_of "input" |> Enum.map String.enum |> Field2D.make

let next_state_p1 fld =
  fld |> Field2D.mapi (fun x y -> function 
  | 'L' -> if Field2D.get3x3 fld x y |> Enum.exists ((=) '#') then 'L' else '#'
  | '#' -> if Field2D.get3x3 fld x y |> Enum.filter ((=) '#') |> Enum.hard_count >= 5 then 'L' else '#'
  | x -> x
  )

let count_when_stable f =
  let rec aux fld =
    let next = f fld in 
    if fld = next then next else aux next in
  aux initial |> Field2D.count_if ((=) '#')

let part1 () =
  count_when_stable next_state_p1

let () = printf "Part 1: %d occupied seats\n%!" (part1 ())
