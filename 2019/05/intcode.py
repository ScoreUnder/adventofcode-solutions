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

    def consumePtrs(self, count, modes):
        return [addr if modes // 10**ind % 10 == 1 else self.program[addr]
                for ind, addr in enumerate(self.consume(count))]

    def writePtr(self, val):
        self.program[self.consume(1)[0]] = val

    def step(self):
        modes, opcode = divmod(self.consume(1)[0], 100)
        if opcode == 1:
            self.writePtr(sum(self.consumePtrs(2, modes)))
        elif opcode == 2:
            val1, val2 = self.consumePtrs(2, modes)
            self.writePtr(val1 * val2)
        elif opcode == 3:
            self.writePtr(int(input("Need input: ")))
        elif opcode == 4:
            print(self.consumePtrs(1, modes))
        elif opcode == 5:
            cond, target = self.consumePtrs(2, modes)
            if cond != 0:
                self.pc = target
        elif opcode == 6:
            cond, target = self.consumePtrs(2, modes)
            if cond == 0:
                self.pc = target
        elif opcode == 7:
            a, b = self.consumePtrs(2, modes)
            self.writePtr(int(a < b))
        elif opcode == 8:
            a, b = self.consumePtrs(2, modes)
            self.writePtr(int(a == b))
        elif opcode == 99:
            self.pc = None
        else:
            raise Exception(f"Bad opcode {opcode}")

    def run(self):
        while self.pc is not None:
            self.step()


def main():
    program = open("input").read().replace('\n', '').split(',')
    program = [int(x) for x in program]
    IntcodeInterpreter(program).run()


main()
