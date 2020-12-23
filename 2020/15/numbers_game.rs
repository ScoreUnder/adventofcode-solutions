use std::vec::Vec;

fn play_game(initial: Vec<i32>, max_turns: i32) -> i32 {
    let mut acc = Vec::new();
    acc.resize_with(max_turns as usize, || -1);
    let max_turns = max_turns - 1;

    let mut turn = 0;
    for value in initial.iter() {
        acc[*value as usize] = turn;
        turn += 1;
    }

    let mut next = 0;
    while turn != max_turns {
        let prev_tn = acc[next as usize];
        let mine = if prev_tn == -1 { 0 } else { turn - prev_tn };
        acc[next as usize] = turn;
        turn += 1;
        next = mine;
    }

    next
}

fn main() {
    println!("Part 1: {}", play_game([8, 11, 0, 19, 1, 2].to_vec(), 2020));
    println!("Part 2: {}", play_game([8, 11, 0, 19, 1, 2].to_vec(), 30000000));
}
