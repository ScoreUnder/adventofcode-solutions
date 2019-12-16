#!/usr/bin/env python3
class IntcodeInterpreter:
    def __init__(self, program):
        assert(all(isinstance(elem, int) for elem in program))
        self.program = program[:]
        self.pc = 0

    def consume(self, count):
        vals = self.program[self.pc:self.pc + count]
        self.pc += count
        return vals

    def consumePtrs(self, count):
        return [self.program[addr] for addr in self.consume(count)]

    def writePtr(self, val):
        self.program[self.consume(1)[0]] = val

    def step(self):
        opcode = self.consume(1)[0]
        if opcode == 1:
            self.writePtr(sum(self.consumePtrs(2)))
        elif opcode == 2:
            val1, val2 = self.consumePtrs(2)
            self.writePtr(val1 * val2)
        elif opcode == 3:
            self.writePtr(int(input("Need input: ")))
        elif opcode == 4:
            print(self.consumePtrs(1))
        elif opcode == 99:
            self.pc = None
        else:
            raise Exception(f"Bad opcode {opcode}")

    def run(self):
        while self.pc is not None:
            self.step()


def execute_loop(program, args):
    interp = IntcodeInterpreter(program)
    interp.program[1:3] = args
    interp.run()

    return interp.program[0]


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
