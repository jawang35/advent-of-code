def part1(depths):
    return sum([
        1
        for a, b in zip(depths, depths[1:])
        if b > a
    ])


def part2(depths):
    sum_of_depths = [
        a + b + c
        for a, b, c in zip(depths, depths[1:], depths[2:])
    ]
    return sum([
        1
        for a, b in zip(sum_of_depths, sum_of_depths[1:])
        if b > a
    ])


if __name__ == '__main__':
    with open('sampleinputs/day01.txt') as f:
        depths = [int(line) for line in f]

    print(f'Part 1: {part1(depths)}')
    print(f'Part 2: {part2(depths)}')
