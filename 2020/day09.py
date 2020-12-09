import os


def test_number(number, preamble):
    for j, a in enumerate(preamble):
        for b in preamble[j + 1:]:
            if a + b == number:
                return True

    return False


def find_invalid_number(numbers, preamble_size):
    for i, number in enumerate(numbers):
        if i < preamble_size:
            continue

        preamble = numbers[i - preamble_size:i]
        if not test_number(number, preamble):
            return number


def find_weakness(numbers, invalid_number):
    for i, number in enumerate(numbers):
        current_sum = 0
        j = i
        while current_sum < invalid_number:
            current_sum += numbers[j]
            j += 1

        if current_sum == invalid_number:
            summands = sorted(numbers[i:j])
            return summands[0] + summands[-1]


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day09.txt')) as file:
        numbers = [
            int(line.strip())
            for line in file
        ]
        invalid_number = find_invalid_number(numbers, 25)
        print(f'Part 1: {invalid_number}')

        weakness = find_weakness(numbers, invalid_number)
        print(f'Part 2: {weakness}')
