def part1(commands):
    position = [0, 0]
    for direction, distance in commands:
        if direction == 'up':
            position[1] -= distance
        if direction == 'down':
            position[1] += distance
        if direction == 'forward':
            position[0] += distance

    return position[0] * position[1]


def part2(commands):
    aim = 0
    position = [0, 0]
    for direction, distance in commands:
        if direction == 'up':
            aim -= distance
        if direction == 'down':
            aim += distance
        if direction == 'forward':
            position[0] += distance
            position[1] += aim * distance

    return position[0] * position[1]


if __name__ == '__main__':
    with open('sampleinputs/day02.txt') as f:
        commands = [
            (direction, int(distance))
            for direction, distance in [
                tuple(line.strip().split())
                for line in f
            ]
        ]

    print(f'Part 1: {part1(commands)}')
    print(f'Part 2: {part2(commands)}')
