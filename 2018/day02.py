def checksum(ids):
    doubles = 0
    triples = 0
    for id in ids:
        chars = {}
        for char in id:
            if char in chars:
                chars[char] += 1
            else:
                chars[char] = 1
        char_counts = set(chars.values())
        if 2 in char_counts:
            doubles += 1
        if 3 in char_counts:
            triples += 1
    return doubles * triples


def common(string1, string2):
    result = ''
    for i, char1 in enumerate(string1):
        if char1 == string2[i]:
            result += char1
    return result


def correct_ids_common(ids):
    for i, id1 in enumerate(ids):
        correct_length = len(id1) - 1
        commons = [common(id1, id2)
                   for id2 in ids[(i + 1):]]
        correct = next(( c for c in commons if len(c) == correct_length ), None)
        if correct:
            return correct


if __name__ == '__main__':
    with open('2018/sampleinputs/day02.txt') as file:
        ids = file.read().split()

        part1 = checksum(ids)
        print('Part 1: {}'.format(part1))

        part2 = correct_ids_common(ids)
        print('Part 2: {}'.format(part2))
