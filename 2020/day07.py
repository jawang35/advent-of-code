import os
import re

CHILD_REGEX = re.compile(r'(\d+) ([a-z]+ [a-z]+) bag(s\.)?')


def parse_line(line):
    contain_index = line.find('contain')
    parent = line[:contain_index - 1].replace(' bags', '')
    children = {
        child_match.group(2): int(child_match.group(1))
        for child_match in [
            CHILD_REGEX.match(child)
            for child in line[contain_index + 8:].split(', ')
            if 'no other bags' not in child
        ]
    }
    return parent, children


def find(bag_graph, parent, color):
    children = bag_graph[parent]
    if color in children:
        return True

    return any(
        find(bag_graph, child, color)
        for child in children
    )


def count_bags(bag_graph, color):
    children = bag_graph[color]
    return sum(
        child_count + (child_count * count_bags(bag_graph, child_color))
        for child_color, child_count in children.items()
    )


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day07.txt')) as file:
        bag_graph = {
            parent: child
            for parent, child in [
                parse_line(line.strip())
                for line in file
            ]
        }

        part1 = len([
            parent
            for parent in bag_graph.keys()
            if find(bag_graph, parent, 'shiny gold')
        ])
        print(f'Part 1: {part1}')

        part2 = count_bags(bag_graph, 'shiny gold')
        print(f'Part 2: {part2}')
