if __name__ == '__main__':
    with open('2018/sampleinputs/day1.txt') as file:
        changes = [int(i) for i in file.read().split()]

        part1 = sum(changes)
        print('Part 1: {}'.format(part1))

        part2 = 0
        frequencies = set()
        i = 0
        length = len(changes)
        while True:
            if part2 in frequencies:
                break
            frequencies.add(part2)
            part2 += changes[i % length]
            i += 1

        print('Part 2: {}'.format(part2))
