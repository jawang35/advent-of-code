import re

class Point():
    def __init__(self, values):
        self.position = (values[0], values[1])
        self.velocity = (values[2], values[3])

    def move(self):
        self.position = (self.position[0] + self.velocity[0],
                         self.position[1] + self.velocity[1])

    def rewind(self):
        self.position = (self.position[0] - self.velocity[0],
                         self.position[1] - self.velocity[1])


def area(points):
    top = min([p.position[1] for p in points])
    bottom = max([p.position[1] for p in points])
    left = min([p.position[0] for p in points])
    right = max([p.position[0] for p in points])
    return (bottom - top + 1) * (right - left + 1)


def draw(points):
    top = min([p.position[1] for p in points])
    bottom = max([p.position[1] for p in points])
    left = min([p.position[0] for p in points])
    right = max([p.position[0] for p in points])
    plane = [None] * (bottom - top + 1)
    for i, _ in enumerate(plane):
        plane[i] = [False] * (right - left + 1)

    for point in points:
        plane[point.position[1] - top][point.position[0] - left] = True

    print('Part 1:')
    for row in plane:
        print(['X' if point else ' ' for point in row])


def move_until_message_appears(points):
    seconds = -1
    current_area = 1
    next_area = 0
    while current_area > next_area:
        seconds += 1
        current_area = area(points)
        for point in points:
            point.move()
        next_area = area(points)

    for point in points:
        point.rewind()

    draw(points)
    return seconds


if __name__ == '__main__':
    with open('2018/sampleinputs/day10.txt') as file:
        point_regex = re.compile(r'-?\d+')
        points = [Point([int(value) for value in point_regex.findall(line)])
                  for line in file.read().strip().split('\n')]

        seconds = move_until_message_appears(points)
        print('Part 2: {}'.format(seconds))
