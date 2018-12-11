def define_contained_area(coordinates):
    xs = [x for (x, _) in coordinates]
    ys = [y for (_, y) in coordinates]
    bottom_right = (max(xs), max(ys))
    contained_area = [None] * (bottom_right[0] + 1)
    for x, _ in enumerate(contained_area):
        contained_area[x] = [None] * (bottom_right[1] + 1)
    return contained_area


def taxicab_distance(coord1, coord2):
    x1, y1 = coord1
    x2, y2 = coord2
    return abs(y1 - y2) + abs(x1 - x2)


def draw_dangerous_coordinate_areas(coordinates):
    contained_area = define_contained_area(coordinates)

    for x, column in enumerate(contained_area):
        for y, _ in enumerate(column):
            taxicab_distances = [(i, taxicab_distance(c, (x, y)))
                                for i, c in enumerate(coordinates)]
            [closest, second_closest] = sorted(taxicab_distances, key=lambda x: x[1])[0:2]
            if closest[1] < second_closest[1]:
                column[y] = closest[0]

    return contained_area


def area_safe_region(coordinates):
    contained_area = define_contained_area(coordinates)

    area = 0
    for x, column in enumerate(contained_area):
        for y, _ in enumerate(column):
            taxicab_distances = [taxicab_distance(c, (x, y))
                                 for c in coordinates]
            if sum(taxicab_distances) < 10000:
                area += 1

    return area


def largest_dangerous_area(coordinates):
    contained_area = draw_dangerous_coordinate_areas(coordinates)

    coordinates_with_infinite_areas = set()
    for x, column in enumerate(contained_area):
        if x == 0:
            for coordinate_index in column:
                coordinates_with_infinite_areas.add(coordinate_index)
        elif x == len(contained_area) - 1:
            for coordinate_index in column:
                coordinates_with_infinite_areas.add(coordinate_index)
        else:
            coordinates_with_infinite_areas.add(column[0])
            coordinates_with_infinite_areas.add(column[-1])

    areas = [0] * len(coordinates)
    for x, column in enumerate(contained_area):
        for y, closest in enumerate(column):
            if closest is not None and not closest in coordinates_with_infinite_areas:
                areas[closest] += 1

    return max(areas)


if __name__ == '__main__':
    with open('2018/sampleinputs/day6.txt') as file:
        coordinates = [tuple([int(x.strip()) for x in line.split(',')])
                       for line in file.read().strip().split('\n')]

        part1 = largest_dangerous_area(coordinates)
        print('Part 1: {}'.format(part1))

        part2 = area_safe_region(coordinates)
        print('Part 2: {}'.format(part2))
