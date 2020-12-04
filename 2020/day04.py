import os
import re

EYR_REGEX = re.compile(r'^(\d+)(cm|in)$')
HCL_REGEX = re.compile(r'^#([0-9]|[a-f]){6}$')
ECL_SET = {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'}
PID_REGEX = re.compile(r'^\d{9}$')


def valid_hgt(x):
    match = EYR_REGEX.match(x)
    if not match:
        return False

    value = int(match.group(1))
    unit = match.group(2)

    if unit == 'cm':
        return 150 <= value <= 193
    elif unit == 'in':
        return 59 <= value <= 76
    else:
        return False


EXPECTED_FIELDS = [
    ('byr', lambda x: len(x) == 4 and 1920 <= int(x) <= 2002),
    ('iyr', lambda x: len(x) == 4 and 2010 <= int(x) <= 2020),
    ('eyr', lambda x: len(x) == 4 and 2020 <= int(x) <= 2030),
    ('hgt', valid_hgt),
    ('hcl', lambda x: HCL_REGEX.match(x)),
    ('ecl', lambda x: x in ECL_SET),
    ('pid', lambda x: PID_REGEX.match(x)),
]


def valid_passport(passport, with_check=False):
    return all(
        field in passport and (not with_check or check(passport[field]))
        for field, check in EXPECTED_FIELDS
    )


if __name__ == '__main__':
    with open(os.path.join('sampleinputs', 'day04.txt')) as file:
        passports = []
        current_passport = {}
        for line in file:
            if line.strip() == '':
                passports.append(current_passport)
                current_passport = {}
                continue

            key_values = [
                kv.split(':')
                for kv in line.strip().split(' ')
            ]
            current_passport.update({
                key: value
                for key, value in key_values
            })

        passports.append(current_passport)

        part1 = len([
            passport
            for passport in passports
            if valid_passport(passport)
        ])
        print(f'Part 1: {part1}')

        part2 = len([
            passport
            for passport in passports
            if valid_passport(passport, with_check=True)
        ])
        print(f'Part 2: {part2}')
