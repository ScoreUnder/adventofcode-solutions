def play_game(initial, max_turns):
    acc = [-1] * max_turns
    max_turns -= 1

    for turn, itm in enumerate(initial):
        acc[itm] = turn

    tn = len(initial)
    next = 0
    while tn != max_turns:
        prev_tn = acc[next]
        mine = 0 if prev_tn == -1 else tn - prev_tn
        acc[next] = tn
        tn += 1
        next = mine

    return next


print("Part 1:", play_game([8, 11, 0, 19, 1, 2], 2020))
print("Part 2:", play_game([8, 11, 0, 19, 1, 2], 30000000))
