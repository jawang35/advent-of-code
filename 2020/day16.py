import os
import re

RULE_REGEX = re.compile(r'(.+): (\d+)-(\d+) or (\d+)-(\d+)')
DEPARTURE_REGEX = re.compile(r'^departure')


def is_valid(value, rule1, rule2):
    return (rule1[0] <= value <= rule1[1]) or (rule2[0] <= value <= rule2[1])


def filter_tickets(tickets, rules):
    error_rate = 0
    valid_tickets = []
    for ticket in nearby_tickets:
        valid = True
        for value in ticket:
            if all(
                not is_valid(value, rule1, rule2)
                for rule1, rule2 in rules.values()
            ):
                valid = False
                error_rate += value

        if valid:
            valid_tickets.append(ticket)

    return valid_tickets, error_rate


def parse_ticket_fields(tickets, rules):
    possible_fields = {
        rule: set(range(len(tickets)))
        for rule in rules.keys()
    }

    # Narrow possible field sets by checking ticket values against rules.
    unknown_positions = set(range(len(tickets)))
    for ticket in tickets:
        for i, value in enumerate(ticket):
            if i not in unknown_positions:
                continue

            for rule, fields in possible_fields.items():
                if len(fields) == 1:
                    continue

                rule1, rule2 = rules[rule]
                if not is_valid(value, rule1, rule2):
                    # Remove field position if ticket has values not valid for rule.
                    fields.remove(i)

                    # If only one possible field remains for rule, remove from
                    # other rules.
                    if len(fields) == 1:
                        found_position = list(fields)[0]
                        unknown_positions.remove(found_position)
                        for other_rule, other_field in possible_fields.items():
                            if rule == other_rule:
                                continue

                            other_field.remove(found_position)

    # Narrow possible field sets by looking for field positions found in
    # single rule until all rules have single field.
    fields_parsed = False
    while not fields_parsed:
        fields_parsed = True
        for rule, fields in possible_fields.items():
            if len(fields) == 1:
                continue

            fields_parsed = False
            other_rules_fields = [
                other_fields
                for other_rule, other_fields in possible_fields.items()
                if other_rule != rule
            ]
            for field in fields:
                if all(
                    field not in other_fields
                    for other_fields in other_rules_fields
                ):
                    possible_fields[rule] = set([field])
                    break

    return [
        rule
        for rule, _ in sorted(
            possible_fields.items(),
            key=lambda x: list(x[1])[0],
        )
    ]


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day16.txt')) as file:
        sections = file.read().strip().split('\n\n')
        [rules, ticket, nearby_tickets] = sections
        rules = {
            match[1]: (
                (int(match[2]), int(match[3])),
                (int(match[4]), int(match[5])),
            )
            for match in [
                RULE_REGEX.match(rule)
                for rule in sections[0].split('\n')
            ]
        }
        ticket = [
            int(value)
            for value in sections[1].split('\n')[1].split(',')
        ]
        nearby_tickets = [
            [int(value) for value in ticket.split(',')]
            for ticket in sections[2].split('\n')[1:]
        ]

        valid_tickets, error_rate = filter_tickets(nearby_tickets, rules)
        print(f'Part 1: {error_rate}')

        ticket_fields = parse_ticket_fields(valid_tickets, rules)
        departure_product = 1
        for i, field in enumerate(ticket_fields):
            if not DEPARTURE_REGEX.match(field):
                continue

            departure_product *= ticket[i]

        print(f'Part 2: {departure_product}')
