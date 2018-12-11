class Marble():
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left or self
        self.right = right or self


def play(players, last_marble):
    player_points = [0] * players
    current_marble = Marble(0)
    for n in range(1, last_marble + 1):
        if n % 23 == 0:
            player = (n % players) - 1
            player_points[player] += n

            # move counter-clockwise 7 marbles
            for _ in range(7):
                current_marble = current_marble.left

            # remove marble
            current_marble.left.right = current_marble.right
            current_marble.right.left = current_marble.left

            player_points[player] += current_marble.value
            current_marble = current_marble.right
        else:
            # insert marble between 1 and 2 marbles clockwise
            left = current_marble.right
            right = left.right
            current_marble = Marble(n, left, right)
            left.right = current_marble
            right.left = current_marble

    return player_points


if __name__ == '__main__':
    with open('2018/sampleinputs/day9.txt') as file:
        words = file.read().strip().split(' ')
        players = int(words[0])
        last_marble = int(words[6])

        part1 = max(play(players, last_marble))
        print('Part 1: {}'.format(part1))

        part2 = max(play(players, 100 * last_marble))
        print('Part 2: {}'.format(part2))
