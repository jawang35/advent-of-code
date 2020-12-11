import os


def brute_force_count_arrangements(sorted_adapter_joltage_ratings):
    if len(sorted_adapter_joltage_ratings) == 1:
        return 1

    return sum(
        brute_force_count_arrangements(sorted_adapter_joltage_ratings[1 + i:])
        for i, joltage in enumerate(sorted_adapter_joltage_ratings[1:4])
        if joltage - sorted_adapter_joltage_ratings[0] < 4
    )


def count_arrangements(jolt_deltas):
    total = 1
    consecutives_to_arrangements = {
        1: 1,
        2: 1,
    }
    current_consecutives = 1
    for delta in jolt_deltas:
        if delta == 3:
            if current_consecutives in consecutives_to_arrangements:
                total *= consecutives_to_arrangements[current_consecutives]
            else:
                consecutives_to_arrangements[current_consecutives] = (
                    brute_force_count_arrangements(
                        list(range(current_consecutives)),
                    ))
                total *= consecutives_to_arrangements[current_consecutives]

            current_consecutives = 1
        else:
            current_consecutives += 1

    return total


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day10.txt')) as file:
        adapter_joltage_ratings = [0] + sorted([
            int(line.strip())
            for line in file
        ])
        adapter_joltage_ratings.append(adapter_joltage_ratings[-1] + 3)
        jolt_deltas = [
            b - a
            for a, b in zip(
                adapter_joltage_ratings,
                adapter_joltage_ratings[1:],
            )
        ]

        part1 = len([
            delta
            for delta in jolt_deltas
            if delta == 1
        ]) * len([
            delta
            for delta in jolt_deltas
            if delta == 3
        ])
        print(f'Part 1: {part1}')

        part2 = count_arrangements(jolt_deltas)
        print(f'Part 2: {part2}')
