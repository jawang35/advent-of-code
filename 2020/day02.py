import os


def valid_password_range(minimum, maximum, char, password):
    return minimum <= password.count(char) <= maximum


def valid_password_position(pos1, pos2, char, password):
    if password[pos1 - 1] == char:
        return not password[pos2 - 1] == char
    else:
        return password[pos2 - 1] == char


if __name__ == '__main__':
    with open(os.path.join('inputs', 'day02.txt')) as file:
        part1 = 0
        part2 = 0
        for line in file:
            policy, char, password = tuple(line.strip('\n').split(' '))
            policy_a, policy_b = policy.split('-')
            char = char[0]
            if valid_password_range(int(policy_a), int(policy_b), char, password):
                part1 += 1

            if valid_password_position(int(policy_a), int(policy_b), char, password):
                part2 += 1

        print(f'Part 1: {part1}')
        print(f'Part 2: {part2}')
