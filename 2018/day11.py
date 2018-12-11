def power_level(x, y, serial_number):
    rack_id = x + 10
    prelim_power_level = str(((rack_id * y) + serial_number) * rack_id)
    hundreds_digit = int(prelim_power_level[-3]) if len(prelim_power_level) > 2 else 0
    return hundreds_digit - 5


def build_power_grid(serial_number):
    power_grid = [None] * 300
    for i, _ in enumerate(power_grid):
        power_grid[i] = [power_level(i + 1, j + 1, serial_number)
                   for j in range(300)]
    return power_grid


def square_with_largest_total_power(power_grid, size):
    max_total_power = 0
    top_left = (1, 1)
    for i in range(300 - size):
        for j in range(300 - size):
            total_power = sum([power_grid[i + x][j + y]
                               for x in range(size)
                               for y in range(size)])
            if total_power > max_total_power:
                max_total_power = total_power
                top_left = (i + 1, j + 1)
    return top_left, max_total_power


def largest_total_power_all_sizes(power_grid):
    max_total_power = 0
    max_power_size = 0
    max_top_left = None
    for size in range(1, 20):
        top_left, total_power = square_with_largest_total_power(power_grid, size)
        if total_power > max_total_power:
            max_total_power = total_power
            max_power_size = size
            max_top_left = top_left
    return max_top_left, max_power_size


if __name__ == '__main__':
    serial_number = 8772
    power_grid = build_power_grid(serial_number)

    part1, _ = square_with_largest_total_power(power_grid, 3)
    print('Part 1: {}'.format(part1))

    part2 = largest_total_power_all_sizes(power_grid)
    print('Part 2: ({},{},{})'.format(part2[0][0], part2[0][1], part2[1]))
