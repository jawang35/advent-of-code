from argparse import ArgumentParser


class Unit():
    def __init__(self, type, x, y, ap=3):
        self.type = type
        self.x = x
        self.y = y
        self.ap = ap
        self.hp = 200

    def __repr__(self):
        return '{} ({},{} HP: {}, AP: {})'.format(self.type, self.x, self.y, self.hp, self.ap)


class Path():
    def __init__(self, x, y, previous_node=None):
        self.x = x
        self.y = y
        self.previous_node = previous_node

    def steps(self):
        current_node = [(self.x, self.y)]
        if self.previous_node:
            return self.previous_node.steps() + current_node
        return current_node


def shortest_path_to_enemy(unit, field):
    enemy_type = 'G' if unit.type == 'E' else 'E'
    visited = set([(unit.x, unit.y)])
    paths = [Path(unit.x, unit.y)]
    paths_to_enemy = []
    while paths and not paths_to_enemy:
        new_paths = []
        for path in paths:
            for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
                x, y = path.x + dx, path.y + dy
                tile = field[y][x]
                if ((x, y) not in visited and tile == '.') or tile == enemy_type:
                    new_paths.append(Path(x, y, path))
                    visited.add((x, y))

        paths = new_paths
        paths_to_enemy = [path for path in paths if field[path.y][path.x] == enemy_type]

    sorted_paths_to_enemy = sorted(
        [path.steps() for path in paths_to_enemy],
        key=lambda steps: (steps[1][1], steps[1][0]),
    )
    return sorted_paths_to_enemy[0] if sorted_paths_to_enemy else []


def weakest_enemy_in_range(unit, enemy_units):
    positions_in_range = set([(unit.x + x, unit.y + y)
                              for (x, y) in [(0, -1), (-1, 0), (1, 0), (0, 1)]])
    enemies_in_range = [enemy
                        for position, enemy in enemy_units.items()
                        if position in positions_in_range]
    return sorted(enemies_in_range, key=lambda enemy: enemy.hp)[0] if enemies_in_range else None


def draw(round, field, elves, goblins):
    print('Round: {}'.format(round))
    for y, row in enumerate(field):
        for x, tile in enumerate(row):
            print(tile, end='')
        elves_in_row = [elf for elf in elves.values() if elf.y == y]
        goblins_in_row = [goblin for goblin in goblins.values() if goblin.y == y]
        units = sorted(elves_in_row + goblins_in_row, key=lambda unit: unit.x)
        print('   {}'.format(units if units else '').rstrip())
    print('')


def parse_battle(input, elf_ap=3):
    field = [[tile for tile in row]
             for row in input.split('\n')]

    elves = {(x, y): Unit(tile, x, y, elf_ap)
             for y, row in enumerate(field)
             for x, tile in enumerate(row)
             if tile == 'E'}

    goblins = {(x, y): Unit(tile, x, y)
               for y, row in enumerate(field)
               for x, tile in enumerate(row)
               if tile == 'G'}

    return field, elves, goblins


def begin_battle(input, elf_ap=3, debug=False):
    field, elves, goblins = parse_battle(input, elf_ap)
    round = -1
    while elves and goblins:
        round += 1
        unit_positions = sorted(list(elves.keys()) + list(goblins.keys()), key=lambda position: (position[1], position[0]))
        for position in unit_positions:
            ally_units = elves if field[position[1]][position[0]] == 'E' else goblins
            enemy_units = goblins if field[position[1]][position[0]] == 'E' else elves
            if position not in ally_units:
                continue
            unit = ally_units[position]

            # move if path to enemy found
            path_to_enemy = shortest_path_to_enemy(unit, field)
            if len(path_to_enemy) > 2:
                new_position = path_to_enemy[1]
                field[unit.y][unit.x] = '.'
                del ally_units[position]
                unit.x = new_position[0]
                unit.y = new_position[1]
                field[unit.y][unit.x] = unit.type
                ally_units[new_position] = unit

            # attack weakest enemy within range
            enemy_unit = weakest_enemy_in_range(unit, enemy_units)
            if enemy_unit:
                enemy_unit.hp -= unit.ap
                if enemy_unit.hp <= 0:
                    field[enemy_unit.y][enemy_unit.x] = '.'
                    del enemy_units[(enemy_unit.x, enemy_unit.y)]
                    if not enemy_units:
                        if debug:
                            draw(round, field, elves, goblins)
                        return round, elves if elves else goblins

        if debug:
            draw(round, field, elves, goblins)

def rigged_battle(input, debug=False):
    _, elves, _ = parse_battle(input)
    elf_ap = 4
    while True:
        round, victors = begin_battle(input, elf_ap, debug)
        victors_units = list(victors.values())
        if victors_units[0].type == 'E' and len(victors_units) == len(elves):
            return round, victors
        elf_ap += 1


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('--debug', action='store_true')
    args = parser.parse_args()
    with open('2018/sampleinputs/day15.txt') as file:
        input = file.read().strip()

        rounds, victors = begin_battle(input, debug=args.debug)
        part1 = rounds * sum([unit.hp for unit in victors.values()])
        print('Part 1: {}'.format(part1))

        rigged_rounds, elves_left = rigged_battle(input, debug=args.debug)
        part2 = rigged_rounds * sum([unit.hp for unit in elves_left.values()])
        print('Part 2: {}'.format(part2))
