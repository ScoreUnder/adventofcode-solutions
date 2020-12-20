open Batteries
open Printf
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let bags_line_re = {|^(\w+ \w+) bags contain|} |> Re.Perl.re |> Re.compile

let bags_sub_re = {|(\d+) (\w+ \w+) bag|} |> Re.Perl.re |> Re.compile

let bags_sub_groups group =
  (int_of_string (Re.Group.get group 1), Re.Group.get group 2)

let bag_defs =
  File.lines_of "input"
  |> Enum.map (fun line ->
         let parent = Re.(Group.get (exec bags_line_re line)) 1 in
         let children =
           Re.all ~pos:(String.length parent + 1) bags_sub_re line
           |> List.map bags_sub_groups
         in
         (parent, children))
  |> List.of_enum

let stringmap_add_to_list k v map =
  StringMap.add k (v :: StringMap.find_default [] k map) map

let bag_immediate_parents =
  bag_defs
  |> List.fold_left
       (fun acc (parent, children) ->
         children
         |> List.fold_left
              (fun acc (_, child_name) ->
                stringmap_add_to_list child_name parent acc)
              acc)
       StringMap.empty

let count_potential_parents key =
  let rec aux keys visited =
    match keys with
    | [] -> visited
    | h :: t when StringSet.mem h visited -> aux t visited
    | h :: t ->
        aux
          (List.rev_append
             (StringMap.find_default [] h bag_immediate_parents)
             t)
          (StringSet.add h visited)
  in
  StringSet.cardinal (aux [ key ] StringSet.empty) - 1

let part1 which =
  printf "%s has %d parents\n" which (count_potential_parents which)

let () = part1 "shiny gold"
