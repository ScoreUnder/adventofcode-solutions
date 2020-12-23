use std::collections::HashMap;
use std::vec::Vec;

fn play_game(initial: Vec<u32>, max_turns: u32) -> u32 {
    let mut acc: HashMap<u32, u32> = HashMap::new();
    let max_turns = max_turns - 1;

    let mut turn = 0u32;
    for value in initial.iter() {
        acc.insert(*value, turn);
        turn += 1;
    }

    let mut next = 0u32;
    while turn != max_turns {
        let mine = turn - acc.get(&next).unwrap_or(&turn);
        acc.insert(next, turn);
        turn += 1;
        next = mine;
    }

    next
}

fn main() {
    println!("Part 1: {}", play_game([8, 11, 0, 19, 1, 2].to_vec(), 2020));
    println!("Part 2: {}", play_game([8, 11, 0, 19, 1, 2].to_vec(), 30000000));
}