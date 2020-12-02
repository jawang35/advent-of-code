import os

def part1(entries):
    for i, a in enumerate(entries):
        for b in entries[(i+1):]:
            if a + b == 2020:
                return a * b

def part2(entries):
    for i, a in enumerate(entries):
        for j, b in enumerate(entries[(i+1):]):
            for c in entries:
                if a + b + c == 2020:
                    return a * b * c


if __name__ == '__main__':
    with open(os.path.join('inputs', 'day01.txt')) as file:
        entries = [
            int(entry)
            for entry in file.read().split()
        ]

        print(f'Part 1: {part1(entries)}')
        print(f'Part 2: {part2(entries)}')
