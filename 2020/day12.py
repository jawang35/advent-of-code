import os


def parse_instruction(instruction):
    action = instruction[0]
    value = int(instruction[1:]) if len(instruction) > 1 else None
    return action, value


def manhattan_distance(position):
    x, y = position
    return abs(x) + abs(y)


def navigate_instructions(instructions):
    current_direction = 270
    current_position = (0, 0)
    for action, value in instructions:
        x, y = current_position
        if action == 'N':
            current_position = (x, y + value)
        elif action == 'S':
            current_position = (x, y - value)
        elif action == 'E':
            current_position = (x - value, y)
        elif action == 'W':
            current_position = (x + value, y)
        elif action == 'L':
            current_direction = (current_direction + value) % 360
        elif action == 'R':
            current_direction = (current_direction - value) % 360
        elif action == 'F':
            if current_direction == 0:
                current_position = (x, y + value)
            elif current_direction == 180:
                current_position = (x, y - value)
            elif current_direction == 270:
                current_position = (x - value, y)
            elif current_direction == 90:
                current_position = (x + value, y)

    return manhattan_distance(current_position)


def navigate_instructions_by_waypoints(instructions):
    current_waypoint = (-10, 1)
    current_position = (0, 0)
    for action, value in instructions:
        x, y = current_position
        x_delta, y_delta = current_waypoint
        rotate = 0
        if action == 'N':
            current_waypoint = (x_delta, y_delta + value)
        elif action == 'S':
            current_waypoint = (x_delta, y_delta - value)
        elif action == 'E':
            current_waypoint = (x_delta - value, y_delta)
        elif action == 'W':
            current_waypoint = (x_delta + value, y_delta)
        elif action == 'L':
            rotate = value % 360
        elif action == 'R':
            rotate = -value % 360
        elif action == 'F':
            current_position = (x + (value * x_delta), y + (value * y_delta))

        if rotate == 0:
            pass
        elif rotate == 90:
            current_waypoint = (y_delta, -x_delta)
        elif rotate == 180:
            current_waypoint = (-x_delta, -y_delta)
        elif rotate == 270:
            current_waypoint = (-y_delta, x_delta)

    return manhattan_distance(current_position)


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day12.txt')) as file:
        instructions = [
            parse_instruction(line.strip())
            for line in file
        ]
        part1 = navigate_instructions(instructions)
        print(f'Part 1: {part1}')

        part2 = navigate_instructions_by_waypoints(instructions)
        print(f'Part 2: {part2}')
