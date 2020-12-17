import copy
import os


def find_adjacent_seats(seat_layout, row, column, observed=False):
    adjacent_seat_indexes = [
        (row_delta, column_delta)
        for row_delta in [-1, 1, 0]
        for column_delta in [-1, 1, 0]
        if (row_delta != 0 or column_delta != 0)
    ]

    if observed:
        for i, (row_delta, column_delta) in enumerate(adjacent_seat_indexes):
            j = 1
            while (
                0 <= row + (j * row_delta) < len(seat_layout) and
                0 <= column + (j * column_delta) < len(seat_layout[row + (j * row_delta)]) and
                seat_layout[row + (j * row_delta)][column + (j * column_delta)] == '.'
            ):
                j += 1

            adjacent_seat_indexes[i] = (j * row_delta, j * column_delta)

    return [
        seat_layout[row + row_delta][column + column_delta]
        for row_delta, column_delta in adjacent_seat_indexes
        if 0 <= row + row_delta < len(seat_layout)
        if 0 <= column + column_delta < len(seat_layout[row + row_delta])
    ]


def simulate_seats(seat_layout, observed, occupied_threshold):
    current_seat_layout = copy.deepcopy(seat_layout)
    changed = True
    while changed:
        changed = False
        previous_seat_layout = copy.deepcopy(current_seat_layout)
        for i, row in enumerate(previous_seat_layout):
            for j, seat in enumerate(row):
                occupied_adjacent_seats = sum(
                    1
                    for seat in find_adjacent_seats(previous_seat_layout, i, j, observed)
                    if seat == '#'
                )
                if seat == 'L' and occupied_adjacent_seats == 0:
                    current_seat_layout[i][j] = '#'
                    changed = True
                elif seat == '#' and occupied_adjacent_seats >= occupied_threshold:
                    current_seat_layout[i][j] = 'L'
                    changed = True

    return current_seat_layout


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day11.txt')) as file:
        seat_layout = [
            list(line.strip())
            for line in file
        ]

        part1 = sum(
            1
            for row in simulate_seats(seat_layout, observed=False, occupied_threshold=4)
            for seat in row
            if seat == '#'
        )
        print(f'Part 1: {part1}')

        part2 = sum(
            1 for row in simulate_seats(seat_layout, observed=True, occupied_threshold=5)
            for seat in row
            if seat == '#'
        )
        print(f'Part 2: {part2}')
