from collections import deque


class Cart():
    def __init__(self, direction, x, y):
        self.direction = direction
        self.x = x
        self.y = y
        self.intersection_actions = deque(['LEFT', 'STRAIGHT', 'RIGHT'])

    def location(self):
        return (self.x, self.y)

    def move(self, track):
        if self.direction == '^':
            self.y -= 1
        elif self.direction == 'v':
            self.y += 1
        elif self.direction == '<':
            self.x -= 1
        else:
            self.x += 1

        track_piece = track[self.y][self.x]
        if track_piece == '/':
            if self.direction == '^':
                self.direction = '>'
            elif self.direction == 'v':
                self.direction = '<'
            elif self.direction == '<':
                self.direction = 'v'
            else:
                self.direction = '^'
        elif track_piece == '\\':
            if self.direction == '^':
                self.direction = '<'
            elif self.direction == 'v':
                self.direction = '>'
            elif self.direction == '<':
                self.direction = '^'
            else:
                self.direction = 'v'
        elif track_piece == '+':
            action = self.intersection_actions.popleft()
            self.intersection_actions.append(action)
            if action == 'LEFT':
                if self.direction == '^':
                    self.direction = '<'
                elif self.direction == 'v':
                    self.direction = '>'
                elif self.direction == '<':
                    self.direction = 'v'
                else:
                    self.direction = '^'
            elif action == 'RIGHT':
                if self.direction == '^':
                    self.direction = '>'
                elif self.direction == 'v':
                    self.direction = '<'
                elif self.direction == '<':
                    self.direction = '^'
                else:
                    self.direction = 'v'


def locate_carts(track_with_carts):
    carts = []
    track = []
    for y, row in enumerate(track_with_carts):
        new_row = []
        for x, char in enumerate(row):
            if char in ['^', 'v', '<', '>']:
                carts.append(Cart(char, x, y))
                if char in ['^', 'v']:
                    new_row.append('|')
                else:
                    new_row.append('-')
            else:
                new_row.append(char)
        track.append(new_row)

    return carts, track


def check_for_crashes(carts):
    crash_location = None
    seen_locations = set()
    for cart in carts:
        location = cart.location()
        location_key = '{}'.format(location)
        if location_key in seen_locations:
            return location
        seen_locations.add(location_key)

    return crash_location


def move_until_one_cart_remains(track, carts):
    current_carts = carts
    first_crash_location = None
    while len(current_carts) > 1:
        for cart in sorted(current_carts, key=lambda cart: (cart.y, cart.x)):
            cart.move(track)
            crash_location = check_for_crashes(current_carts)
            if crash_location:
                first_crash_location = first_crash_location or crash_location
                current_carts = [cart
                                 for cart in current_carts
                                 if cart.location() != crash_location]

    return first_crash_location, current_carts[0]


if __name__ == '__main__':
    with open('2018/sampleinputs/day13.txt') as file:
        carts, track = locate_carts(file.read().strip('\n').split('\n'))

        first_crash_location, remaining_cart = move_until_one_cart_remains(track, carts)
        print('Part 1: {}'.format(first_crash_location))
        print('Part 2: {}'.format(remaining_cart.location()))
