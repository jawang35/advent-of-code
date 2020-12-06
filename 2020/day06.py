import os

if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day06.txt')) as file:
        group_answers = []
        current_group = []
        for line in file:
            answers = list(line.strip('\n'))
            if not answers:
                group_answers.append(current_group)
                current_group = []
                continue

            current_group.append(answers)

        group_answers.append(current_group)

        group_answers1 = []
        for group in group_answers:
            current_group_answers = set(group[0])
            for answers in group[1:]:
                current_group_answers = current_group_answers.union(answers)
            group_answers1.append(current_group_answers)

        part1 = sum([
            len(answers)
            for answers in group_answers1
        ])
        print(f'Part 1: {part1}')

        group_answers2 = []
        for group in group_answers:
            current_group_answers = set(group[0])
            for answers in group[1:]:
                current_group_answers = (
                    current_group_answers.intersection(answers)
                )

            group_answers2.append(current_group_answers)

        part2 = sum([
            len(answers)
            for answers in group_answers2
        ])
        print(f'Part 2: {part2}')
