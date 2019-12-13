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


def execute_loop(program):
    pc = 0  # Program Counter
    while pc is not None:
        pc = execute(program, pc)


def main():
    program = open("input").read().replace('\n', '').split(',')
    program = [int(x) for x in program]

    # Weird problem spec but okay:
    program[1:3] = [12, 2]
    execute_loop(program)

    print(program[0])


main()
