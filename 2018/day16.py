import re

def update_register(compute):
    def update(a, b, c, register):
        new_register = register.copy()
        new_register[c] = compute(a, b, c, register)
        return new_register
    return update


opcode_actions = {
    'addr': update_register(lambda a, b, c, r: r[a] + r[b]),
    'addi': update_register(lambda a, b, c, r: r[a] + b),
    'mulr': update_register(lambda a, b, c, r: r[a] * r[b]),
    'muli': update_register(lambda a, b, c, r: r[a] * b),
    'banr': update_register(lambda a, b, c, r: r[a] & r[b]),
    'bani': update_register(lambda a, b, c, r: r[a] & b),
    'borr': update_register(lambda a, b, c, r: r[a] | r[b]),
    'bori': update_register(lambda a, b, c, r: r[a] | b),
    'setr': update_register(lambda a, b, c, r: r[a]),
    'seti': update_register(lambda a, b, c, r: a),
    'gtir': update_register(lambda a, b, c, r: 1 if a > r[b] else 0),
    'gtri': update_register(lambda a, b, c, r: 1 if r[a] > b else 0),
    'gtrr': update_register(lambda a, b, c, r: 1 if r[a] > r[b] else 0),
    'eqir': update_register(lambda a, b, c, r: 1 if a == r[b] else 0),
    'eqri': update_register(lambda a, b, c, r: 1 if r[a] == b else 0),
    'eqrr': update_register(lambda a, b, c, r: 1 if r[a] == r[b] else 0),
}


def parse_register_log(input):
    lines = input.split('\n')
    register_state_regex = re.compile(r'\d, \d, \d, \d')
    before = [int(char) for char in register_state_regex.search(lines[0]).group(0).split(', ')]
    instruction = [int(char) for char in lines[1].split(' ')]
    after = [int(char) for char in register_state_regex.search(lines[2]).group(0).split(', ')]
    return (before, instruction, after)


def matching_opcodes(log):
    (before, [_, a, b, c], after) = log
    return [opcode
            for opcode, action in opcode_actions.items()
            if action(a, b, c, before) == after]


def logs_opcodes_matches(register_log):
    return [(log, matching_opcodes(log)) for log in register_log]


def determine_opcode_numbers(logs_with_opcodes):
    opcodes = [None] * 16
    found_opcodes = set()
    while any((opcode == None for opcode in opcodes)):
        logs_with_unfound_opcodes = [(log, [opcode for opcode in opcodes if opcode not in found_opcodes])
                                     for log, opcodes in logs_with_opcodes
                                     if any((opcode not in found_opcodes for opcode in opcodes))]
        logs_with_single_opcode = [(log, opcodes[0])
                                   for log, opcodes in logs_with_unfound_opcodes
                                   if len(opcodes) == 1]
        for log, opcode in logs_with_single_opcode:
            [opcode_number, _, _, _] = log[1]
            opcodes[opcode_number] = opcode
            found_opcodes.add(opcode)
    return opcodes


def execute(opcode_numbers, test_program):
    register = [0, 0, 0, 0]
    for instruction in test_program:
        [opcode, a, b, c] = instruction
        register = opcode_actions[opcode_numbers[opcode]](a, b, c, register)
    return register


if __name__ == '__main__':
    with open('2018/sampleinputs/day16.txt') as file:
        [input1, input2] = file.read().strip().split('\n\n\n\n')
        register_log = [parse_register_log(line)
                        for line in input1.split('\n\n')]
        test_program = [[int(char) for char in line.split(' ')]
                        for line in input2.split('\n')]

        logs_with_opcodes = logs_opcodes_matches(register_log)

        part1 = len([opcodes
                     for _, opcodes in logs_with_opcodes
                     if len(opcodes) > 2])
        print('Part 1: {}'.format(part1))

        opcode_numbers = determine_opcode_numbers(logs_with_opcodes)
        part2 = execute(opcode_numbers, test_program)[0]
        print('Part 2: {}'.format(part2))
