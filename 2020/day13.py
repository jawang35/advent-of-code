import os
import math


def earliest_departure(target_departure, bus_id):
    departure_time = (math.ceil((target_departure - bus_id) / bus_id) + 1) * bus_id
    return departure_time, departure_time - target_departure


def earliest_timestamp(bus_ids):
    # Solving a system of modulo congruences using Chinese Remainder Theorem:
    # timestamp + minutes = 0 mod bus_id
    timestamp_congruences_modulo_bus_ids = sorted([
        (bus_id, (-minutes_after_t) % bus_id)
        for minutes_after_t, bus_id in enumerate(bus_ids)
        if bus_id is not None
    ], key=lambda x: x[0], reverse=True)

    n_p, t = timestamp_congruences_modulo_bus_ids[0]
    # t = a mod n
    for n, a in timestamp_congruences_modulo_bus_ids[1:]:
        while t % n != a:
            t += n_p

        n_p *= n

    return t


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day13.txt')) as file:
        target_departure = int(file.readline().strip())
        bus_ids = [
            int(bus_id) if bus_id != 'x' else None
            for bus_id in file.readline().strip().split(',')
        ]

        earliest_departure = sorted([
            (bus_id, earliest_departure(target_departure, bus_id))
            for bus_id in bus_ids
            if bus_id is not None
        ], key=lambda x: x[1][0])[0]
        part1 = earliest_departure[0] * earliest_departure[1][1]
        print(f'Part 1: {part1}')

        part2 = earliest_timestamp(bus_ids)
        print(f'Part 2: {part2}')
