def play(players, last_marble):
    player_points = [0] * players
    circle = [0]
    current_position = 0
    player = 0
    for marble in range(1, last_marble + 1):
        if marble % 23 == 0:
            current_position = (current_position - 7) % len(circle)
            points = marble + circle.pop(current_position)
            player_points[player] += points
        else:
            current_position = (current_position + 2) % len(circle)
            if current_position == 0:
                circle.append(marble)
                current_position = len(circle) - 1
            else:
                circle.insert(current_position, marble)
        player = (player + 1) % players
    return player_points

if __name__ == '__main__':
    with open('2018/sampleinputs/day9.txt') as file:
        words = file.read().strip().split(' ')
        players = int(words[0])
        last_marble = int(words[6])

        player_points = play(players, last_marble)
        print('Part 1: {}'.format(max(player_points)))
