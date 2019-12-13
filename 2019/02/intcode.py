#!/usr/bin/env python3
def execute(program, pc):
    opcode = program[pc]
    pc += 1
    if opcode == 1:
        src1, src2, dst = program[pc:pc+3]
        pc += 3
        program[dst] = program[src1] + program[src2]
    elif opcode == 2:
        src1, src2, dst = program[pc:pc+3]
        pc += 3
        program[dst] = program[src1] * program[src2]
    elif opcode == 99:
        return None
    else:
        raise Exception(f"Bad opcode {opcode}")
    return pc


def execute_loop(program, args):
    program = program[:]
    program[1:3] = args

    pc = 0  # Program Counter
    while pc is not None:
        pc = execute(program, pc)

    return program[0]


def main():
    program = open("input").read().replace('\n', '').split(',')
    program = [int(x) for x in program]

    result = execute_loop(program, [12, 2])
    print(result)

    for noun in range(0, len(program)):
        for verb in range(0, len(program)):
            result = execute_loop(program, [noun, verb])
            if result == 19690720:
                print(noun * 100 + verb)
                break


main()
