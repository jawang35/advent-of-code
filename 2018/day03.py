class Claim():
    def __init__(self, claim_string):
        claim_props = claim_string.split()
        self.id = int(claim_props[0][1:])
        position = claim_props[2].split(',')
        self.left = int(position[0])
        self.top = int(position[1][:-1])
        dimensions = claim_props[3].split('x')
        self.width = int(dimensions[0])
        self.height = int(dimensions[1])


def required_sheet_size(claims):
    width = 0
    height = 0
    for claim in claims:
        width = max(width, claim.left + claim.width)
        height = max(height, claim.top + claim.height)
    return width, height


def total_overlap(claims):
    size = required_sheet_size(claims)
    sheet = []
    for x in range(size[0]):
        sheet.append([0] * size[1])

    for claim in claims:
        for x in range(claim.left, claim.left + claim.width):
            for y in range(claim.top, claim.top + claim.height):
                sheet[x][y] += 1

    overlap_size = 0
    for column in sheet:
        for square_inch in column:
            if square_inch > 1:
                overlap_size += 1

    return overlap_size


def is_overlapping(claim1, claim2):
    if claim1.left < claim2.left and claim1.left + claim1.width <= claim2.left:
        return False
    if claim1.top < claim2.top and claim1.top + claim1.height <= claim2.top:
        return False
    if claim2.left < claim1.left and claim2.left + claim2.width <= claim1.left:
        return False
    if claim2.top < claim1.top and claim2.top + claim2.height <= claim1.top:
        return False
    return True


def intact(claims):
    claims_with_overlaps = set()
    for i, claim1 in enumerate(claims):
        for claim2 in claims[(i + 1):]:
            if is_overlapping(claim1, claim2):
                claims_with_overlaps.add(claim1.id)
                claims_with_overlaps.add(claim2.id)

        if claim1.id not in claims_with_overlaps:
            return claim1


if __name__ == '__main__':
    with open('2018/sampleinputs/day03.txt') as file:
        claims = [Claim(c) for c in file.read().split('\n')[:-1]]

        part1 = total_overlap(claims)
        print('Part 1: {}'.format(part1))

        part2 = intact(claims).id
        print('Part 2: {}'.format(part2))
