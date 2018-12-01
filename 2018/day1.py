if __name__ == '__main__':
    with open('sampleinputs/day1.txt') as file:
        changes = [int(line if line[0] != '+' else line[1:])
                   for line in file.read().split()]
        part1 = sum(changes)
        print('Part 1: {}'.format(part1))

        frequencies = set()
        frequency = 0
        i = 0
        length = len(changes)
        while True:
            if frequency in frequencies:
                print(i)
                print('Part 2: {}'.format(frequency))
                break
            frequencies.add(frequency)
            frequency += changes[i % length]
            i += 1

