import os


def seat_id(boarding_pass):
    chars = list(boarding_pass)
    rows = list(range(128))
    for char in chars[:7]:
        half = int(len(rows)/2)
        if char == 'F':
            rows = rows[:half]
        else:
            rows = rows[half:]

    columns = list(range(8))
    for char in chars[7:]:
        half = int(len(columns)/2)
        if char == 'L':
            columns = columns[:half]
        else:
            columns = columns[half:]

    return (rows[0] * 8) + columns[0]


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day05.txt')) as file:
        seat_ids = [
            seat_id(boarding_pass)
            for boarding_pass in file
        ]
        part1 = max(seat_ids)
        print(f'Part 1: {part1}')

        sorted_seats = sorted(seat_ids)
        expected_seats = range(sorted_seats[0], sorted_seats[-1] + 1)
        # Ended up with 2 possible answers ¯\_(ツ)_/¯
        part2 = set(expected_seats) - set(sorted_seats)
        print(f'Part 2: {part2}')
