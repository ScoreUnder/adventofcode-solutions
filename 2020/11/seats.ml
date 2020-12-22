open Batteries
open Printf

module Field2D = struct
  type 'a t = 'a Vect.t Vect.t

  let width (fld : 'a t) = Vect.get fld 0 |> Vect.length

  let height (fld : 'a t) = Vect.length fld

  let make e : 'a t =
    let res = e |> map Vect.of_enum |> Vect.of_enum in
    let w = width res in
    assert (Vect.for_all (fun el -> Vect.length el = w) res);
    res

  let get (fld : 'a t) x y = Vect.get (Vect.get fld y) x

  let get3x3 (fld : 'a t) x y =
    let ys = max 0 (y - 1) -- min (Vect.length fld - 1) (y + 1) in
    let xs = max 0 (x - 1) -- min (Vect.length (Vect.get fld 0) - 1) (x + 1) in
    ys
    |> Enum.concat_map (fun y ->
           let row = Vect.get fld y in
           xs |> Enum.clone |> map (Vect.get row))

  let mapi f (fld : 'a t) : 'a t =
    fld |> Vect.mapi (fun y row -> row |> Vect.mapi (fun x el -> f x y el))

  let show (fld : char t) =
    String.concat "\n"
      (fld |> Vect.map (Vect.enum %> String.of_enum) |> Vect.to_list)

  let count_if f (fld : 'a t) =
    fld |> Vect.enum
    |> map (Vect.enum %> filter f %> Enum.hard_count)
    |> Enum.sum
end

let initial = File.lines_of "input" |> map String.enum |> Field2D.make

let next_state scan fld =
  fld
  |> Field2D.mapi (fun x y -> function
       | 'L' -> if scan fld x y |> exists (( = ) '#') then 'L' else '#'
       | '#' ->
           if scan fld x y |> filter (( = ) '#') |> Enum.hard_count >= 5 then
             'L'
           else '#'
       | x -> x)

let cast_all_rays fld x y =
  let rays =
    let dims = [ -1; 0; 1 ] in
    List.cartesian_product dims dims
    |> List.filter (function 0, 0 -> false | _ -> true)
  in
  let width = Field2D.width fld in
  let height = Field2D.height fld in
  let rec cast_ray (x, y) (dx, dy) =
    if x < 0 || y < 0 || x >= width || y >= height then '.'
    else
      match Field2D.get fld x y with
      | '.' -> cast_ray (x + dy, y + dy) (dx, dy)
      | x -> x
  in
  rays |> List.enum |> map (fun (dx, dy) -> cast_ray (x + dx, y + dy) (x, y))

let count_when_stable f =
  let rec aux fld =
    let next = f fld in
    if fld = next then next else aux next
  in
  aux initial |> Field2D.count_if (( = ) '#')

let part1 () = count_when_stable (next_state Field2D.get3x3)

let part2 () = count_when_stable (next_state cast_all_rays)

let () =
  printf "Part 1: %d occupied seats\n%!" (part1 ());
  printf "Part 2: %d occupied seats\n%!" (part2 ())
