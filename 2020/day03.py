import os


def find_trees(grid, slope):
    row, column = 0, 0
    trees = 0
    while True:
        if grid[row][column] == '#':
            trees += 1

        row += slope[0]
        column = (column + slope[1]) % len(grid[0])

        if row >= len(grid):
            return trees

if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day03.txt')) as file:
        grid = [
            list(line)
            for line in file.read().split()
        ]
        part1 = find_trees(grid, (1, 3))
        print(f'Part 1: {part1}')

        part2_slopes = [
            (1, 1),
            (1, 3),
            (1, 5),
            (1, 7),
            (2, 1),
        ]
        part2 = 1
        for slope in part2_slopes:
            part2 *= find_trees(grid, slope)

        print(f'Part 2: {part2}')
