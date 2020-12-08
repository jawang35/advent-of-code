import os


def parse_instruction(instruction):
    op = instruction[:3]
    sign = instruction[4]
    arg = int(instruction[5:])
    return op, sign, arg


def find_infinite_loop(instructions):
    visited_lines = set()
    accumulator = 0
    line = 0
    while line < len(instructions):
        if line in visited_lines:
            return line, accumulator

        visited_lines.add(line)
        op, sign, arg = parse_instruction(instructions[line])
        if op == 'jmp':
            if sign == '+':
                line += arg
            else:
                line -= arg
            continue

        if op == 'acc':
            if sign == '+':
                accumulator += arg
            else:
                accumulator -= arg

        line += 1

    return -1, accumulator


def fix_program(instructions):
    for i, instruction in enumerate(instructions):
        op, sign, arg = parse_instruction(instruction)

        fixed_instruction = None
        if op == 'jmp':
            fixed_instruction = f'nop {sign}{arg}'
        elif op == 'nop':
            fixed_instruction = f'jmp {sign}{arg}'

        if fixed_instruction:
            new_instructions = instructions[:i] + [fixed_instruction] + instructions[i + 1:]
            line, accumulator = find_infinite_loop(new_instructions)
            if line == -1:
                return i, accumulator

    return -1, accumulator


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day08.txt')) as file:
        instructions = [
            line.strip()
            for line in file
        ]

        _, part1 = find_infinite_loop(instructions)
        print(f'Part 1: {part1}')

        _, part2 = fix_program(instructions)
        print(f'Part 2: {part2}')
