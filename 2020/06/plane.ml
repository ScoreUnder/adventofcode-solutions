open Batteries

module CharSet = Set.Make(Char)

let charset_of_string s =
    let rec aux i set =
        if i < 0 then set else aux (i - 1) (CharSet.add s.[i] set) in
    aux (String.length s - 1) CharSet.empty

let questions =
    File.lines_of "input"
    |> Enum.fold (fun lines v ->
                    if v = ""
                    then [] :: lines
                    else
                      let [@warning "-8"] group :: rest = lines in
                      let ngroup = v :: group in
                        ngroup :: rest)
      [[]]

let sum = List.fold_left (+) 0
let reduce f = function
  | x :: xs -> List.fold_left f x xs
  | [] -> raise (Invalid_argument "empty list passed to reduce")

let sum_assenting_values joiner =
    questions
    |> List.map (fun group ->
        group
        |> List.map charset_of_string
        |> reduce joiner
        |> CharSet.cardinal)
    |> sum

let part1 = sum_assenting_values CharSet.union
let part2 = sum_assenting_values CharSet.inter

let main =
    ignore (Printf.printf "%d\n" part1);
    ignore (Printf.printf "%d\n" part2)

(*let main =
    let print_lst = List.iter (Printf.printf "%s\n") in
    let print_lst2 = List.iter (fun x -> print_lst x; print_endline "") in
    print_lst2 questions*)
