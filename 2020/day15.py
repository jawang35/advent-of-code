import os


def number_spoken(starting_numbers, n):
    last_turns = {
        int(number): [i]
        for i, number in enumerate(starting_numbers)
    }
    last_number = starting_numbers[-1]

    for i in range(len(starting_numbers), n):
        last_number = (
            last_turns[last_number][-1] - last_turns[last_number][-2]
            if last_number in last_turns and len(last_turns[last_number]) > 1
            else 0
        )
        if last_number in last_turns:
            last_turns[last_number].append(i)
        else:
            last_turns[last_number] = [i]

    return last_number


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day15.txt')) as file:
        starting_numbers = file.readline().strip().split(',')

        part1 = number_spoken(starting_numbers, 2020)
        print(f'Part 1: {part1}')

        part2 = number_spoken(starting_numbers, 30_000_000)
        print(f'Part 2: {part2}')
