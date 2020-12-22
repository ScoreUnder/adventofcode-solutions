open Batteries
open Printf

module Field2D = struct
  type 'a t = { v : 'a array array } [@@unboxed]

  let width (fld : 'a t) = fld.v.(0) |> Array.length

  let height (fld : 'a t) = Array.length fld.v

  let make e : 'a t =
    let res = { v = e |> map Array.of_enum |> Array.of_enum } in
    let w = width res in
    assert (Array.for_all (fun el -> Array.length el = w) res.v);
    res

  let get (fld : 'a t) x y = fld.v.(y).(x)

  let get3x3 (fld : 'a t) x y =
    let ys = max 0 (y - 1) -- min (height fld - 1) (y + 1) in
    let xs = max 0 (x - 1) -- min (width fld - 1) (x + 1) in
    ys
    |> Enum.concat_map (fun y ->
           let row = fld.v.(y) in
           xs |> Enum.clone |> map (Array.get row))

  let mapi f (fld : 'a t) : 'b t =
    {
      v =
        fld.v
        |> Array.mapi (fun y row -> row |> Array.mapi (fun x el -> f x y el));
    }

  let iteri f (fld : 'a t) =
    fld.v
    |> Array.iteri (fun y row -> row |> Array.iteri (fun x el -> f x y el))

  let show (fld : char t) =
    String.concat "\n"
      (fld.v |> Array.map (Array.enum %> String.of_enum) |> Array.to_list)

  let count_if f (fld : 'a t) =
    fld.v |> Array.enum
    |> map (Array.enum %> filter f %> Enum.hard_count)
    |> Enum.sum

  let find_mapi (type r) (f : int -> int -> 'a -> r option) (fld : 'a t) : r =
    let exception StopWithValue of r in
    try
      fld
      |> iteri (fun x y el ->
             match f x y el with
             | Some x -> raise (StopWithValue x)
             | None -> ());
      raise Not_found
    with StopWithValue x -> x
end

let initial = File.lines_of "input" |> map String.enum |> Field2D.make

let enum_count_gte f v e =
  let rec aux v =
    if v = 0 then true else if f (Enum.get_exn e) then aux (v - 1) else aux v
  in
  try aux v with Enum.No_more_elements -> false

let next_state scan fld =
  fld
  |> Field2D.mapi (fun x y -> function
       | 'L' -> if scan fld x y |> exists (( = ) '#') then 'L' else '#'
       | '#' ->
           if scan fld x y |> enum_count_gte (( = ) '#') 5 then 'L' else '#'
       | x -> x)

let cast_all_rays fld =
  let rays =
    let dims = [ -1; 0; 1 ] in
    List.cartesian_product dims dims
    |> List.filter (function 0, 0 -> false | _ -> true)
  in
  let width = Field2D.width fld in
  let height = Field2D.height fld in
  let rec cast_ray (x, y) (dx, dy) =
    if x < 0 || y < 0 || x >= width || y >= height then (-1, -1)
    else
      match Field2D.get fld x y with
      | '.' -> cast_ray (x + dy, y + dy) (dx, dy)
      | _ -> (x, y)
  in
  fld
  |> Field2D.mapi (fun x y _ ->
         rays |> List.enum
         |> map (fun (dx, dy) -> cast_ray (x + dx, y + dy) (x, y))
         |> Array.of_enum)

let guaranteed_dot_pos fld =
  fld
  |> Field2D.find_mapi (fun x y elt -> if elt = '.' then Some (x, y) else None)

let preprocess_rays fld =
  let dot_pos = guaranteed_dot_pos fld in
  let oob_to_dot = function -1, -1 -> dot_pos | x -> x in
  fld |> cast_all_rays |> Field2D.mapi (fun _ _ -> Array.map oob_to_dot)

let count_when_stable f =
  let rec aux fld =
    let next = f fld in
    if fld = next then next else aux next
  in
  aux initial |> Field2D.count_if (( = ) '#')

let part1 () = count_when_stable (next_state Field2D.get3x3)

let part2 () =
  let rays = preprocess_rays initial in
  count_when_stable
    (next_state (fun f x y ->
         Field2D.get rays x y |> Array.enum
         |> map (fun (x, y) -> Field2D.get f x y)))

let () =
  printf "Part 1: %d occupied seats\n%!" (part1 ())(*;
  printf "Part 2: %d occupied seats\n%!" (part2 ())*)
