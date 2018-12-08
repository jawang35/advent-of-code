from collections import deque


class Step():
    def __init__(self, label, base_execution_time=0):
        self.label = label
        self.execution_time = ord(label) - ord('A') + base_execution_time + 1

    def __repr__(self):
        return '{}:{}'.format(self.label, self.execution_time)

    def __gt__(self, other_step):
        return self.label > other_step.label

    def execute(self):
        self.execution_time -= 1

    def done(self):
        return self.execution_time <= 0


def steps_and_prereqs(instructions):
    # values are prerequisites to the keys beginning
    steps = set()
    step_prereqs = {}
    for instruction in instructions:
        words = instruction.split(' ')
        prereq = words[1]
        step = words[7]
        steps.add(prereq)
        steps.add(step)
        if step not in step_prereqs:
            step_prereqs[step] = set()
        step_prereqs[step].add(prereq)

    return steps, step_prereqs


def resolve_step_order(instructions):
    steps, step_prereqs = steps_and_prereqs(instructions)
    ranked_steps = sorted(steps)
    ordered_steps = []
    while ranked_steps:
        for i, step in enumerate(ranked_steps):
            next_step = False
            if step not in step_prereqs:
                next_step = True
            elif not step_prereqs[step]:
                next_step = True
                del step_prereqs[step]
            if next_step:
                del ranked_steps[i]
                ordered_steps.append(step)
                for step_with_prereq in step_prereqs.keys():
                    step_prereqs[step_with_prereq].discard(step)
                break

    return ordered_steps


def execute_instructions(instructions, workers, base_execution_time):
    step_labels, step_prereq_labels = steps_and_prereqs(instructions)
    time = 0
    steps_queue = deque()
    current_steps = [None] * workers
    while step_labels or steps_queue or any((step is not None for step in current_steps)):
        time += 1

        # queue steps ready for work
        steps_to_queue = []
        for i, step_label in enumerate(step_labels):
            if step_label not in step_prereq_labels or not step_prereq_labels[step_label]:
                steps_to_queue.append(Step(step_label, base_execution_time))
        for step in sorted(steps_to_queue):
            steps_queue.append(step)
            step_labels.remove(step.label)

        # assign steps to workers from queue
        for i, step in enumerate(current_steps):
            if not step and len(steps_queue):
                current_steps[i] = steps_queue.popleft()

        # execute 1 second of work
        for i, step in enumerate(current_steps):
            if step:
                step.execute()
                if step.execution_time == 0:
                    current_steps[i] = None
                    for step_with_prereq_label in step_prereq_labels.keys():
                        step_prereq_labels[step_with_prereq_label].discard(step.label)

    return time


if __name__ == '__main__':
    with open('2018/sampleinputs/day7.txt') as file:
        instructions = file.read().strip().split('\n')

        part1 = ''.join(resolve_step_order(instructions))
        part2 = execute_instructions(instructions, 5, 60)
        print('Part 1: {}'.format(part1))
        print('Part 2: {}'.format(part2))
