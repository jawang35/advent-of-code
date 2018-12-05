import string


def react(polymer):
    while True:
        initial_length = len(polymer)
        cursor = 0
        while cursor < len(polymer) - 1:
            current_unit = polymer[cursor]
            next_unit = polymer[cursor + 1]
            if current_unit.lower() == next_unit.lower() and current_unit != next_unit:
                del polymer[cursor:(cursor + 2)]
            cursor += 1
        if len(polymer) == initial_length:
            break
    return polymer


def length_fixed_polymer_reaction(polymer):
    fixed_polymer_lengths = [len(react(list(polymer.replace(char, '').replace(char.upper(), ''))))
                      for char in string.ascii_lowercase]

    return min(fixed_polymer_lengths)


if __name__ == '__main__':
    with open('2018/sampleinputs/day5.txt') as file:
        polymer = file.read().strip()

        part1 = len(react(list(polymer)))
        part2 = length_fixed_polymer_reaction(polymer)
        print('Part 1: {}'.format(part1))
        print('Part 2: {}'.format(part2))
