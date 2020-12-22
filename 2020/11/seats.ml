open Batteries
open Printf

module Field2D = struct
  type 'a t = { v : 'a array; w : int; h : int }

  let width (fld : 'a t) = fld.w

  let height (fld : 'a t) = fld.h

  let make e : 'a t =
    let data = e |> map Array.of_enum |> List.of_enum in
    let res =
      {
        v = Array.concat data;
        w = Array.length (List.hd data);
        h = List.length data;
      }
    in
    assert (List.for_all (fun el -> Array.length el = res.w) data);
    assert (Array.length res.v = res.w * res.h);
    res

  let get (fld : 'a t) x y = fld.v.((y * fld.w) + x)

  let get3x3 (fld : 'a t) x y =
    let ys = max 0 (y - 1) -- min (height fld - 1) (y + 1) in
    let xs = max 0 (x - 1) -- min (width fld - 1) (x + 1) in
    ys
    |> Enum.concat_map (fun y ->
           let row = fld.w * y in
           xs |> Enum.clone |> map (fun x -> fld.v.(row + x)))

  let mapi f (fld : 'a t) : 'b t =
    {
      v =
        fld.v
        |> Array.mapi (fun c el ->
               let x = c mod fld.w and y = c / fld.w in
               f x y el);
      w = fld.w;
      h = fld.h;
    }

  let map f (fld : 'a t) : 'b t =
    { v = fld.v |> Array.map f; w = fld.w; h = fld.h }

  let iteri f (fld : 'a t) =
    fld.v
    |> Array.iteri (fun c el ->
           let x = c mod fld.w and y = c / fld.w in
           f x y el)

  let count_if (f : 'a -> bool) (fld : 'a t) = fld.v |> Array.count_matching f
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

let preprocess_rays fld =
  let no_oob = function -1, -1 -> false | x -> true in
  fld |> cast_all_rays |> Field2D.map (Array.filter no_oob)

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
