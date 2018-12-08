from collections import deque


class Tree():
    def __init__(self, tree_input):
        child_nodes_length = tree_input.popleft()
        metadata_entries_length = tree_input.popleft()

        self.child_nodes = [Tree(tree_input) for _ in range(child_nodes_length)]
        self.metadata_entries = [tree_input.popleft() for _ in range(metadata_entries_length)]

    def sum_metadata_entries(self):
        return sum(self.metadata_entries) + sum([cn.sum_metadata_entries()
                                                 for cn in self.child_nodes])

    def root_value(self):
        if self.child_nodes:
            return sum([self.child_nodes[metadata_entry - 1].root_value()
                        for metadata_entry in self.metadata_entries
                        if metadata_entry <= len(self.child_nodes)])

        return sum(self.metadata_entries)


if __name__ == '__main__':
    with open('2018/sampleinputs/day8.txt') as file:
        tree_input = [int(n) for n in file.read().strip().split(' ')]

        tree = Tree(deque(tree_input))
        part1 = tree.sum_metadata_entries()
        part2 = tree.root_value()
        print('Part 1: {}'.format(part1))
        print('Part 2: {}'.format(part2))
