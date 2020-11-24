from datetime import datetime
from enum import Enum, auto
import re

guard_id_regex = re.compile(r'#\d+')


class Record():
    def __init__(self, record_string):
        self.timestamp = datetime.strptime(record_string[1:17], '%Y-%m-%d %H:%M')
        guard_id = guard_id_regex.search(record_string)
        self.guard_id = guard_id[0][1:] if guard_id else None
        if 'wakes up' in record_string:
            self.type = 'WAKES_UP'
        elif 'falls asleep' in record_string:
            self.type = 'FALLS_ASLEEP'
        else:
            self.type = 'BEGINS_SHIFT'


def track_sleep_minutes(minutes_already_slept, start_minute, end_minute):
    for minute in range(start_minute, end_minute):
        minutes_already_slept[minute] += 1


def build_sleep_log(records):
    sleep_log = {}
    current_guard_id = records[0].guard_id
    asleep_since = None
    for record in records:
        if record.type == 'BEGINS_SHIFT':
            current_guard_id = record.guard_id
            asleep_since = None
        if record.type == 'FALLS_ASLEEP':
            asleep_since = record.timestamp
        if record.type == 'WAKES_UP':
            time_asleep = record.timestamp - asleep_since
            if current_guard_id in sleep_log:
                sleep_log[current_guard_id]['time_asleep'] += time_asleep
            else:
                sleep_log[current_guard_id] = {'time_asleep': time_asleep, 'minutes': [0] * 60}
            for minute in range(asleep_since.minute, record.timestamp.minute):
                sleep_log[current_guard_id]['minutes'][minute] += 1

    return sleep_log


if __name__ == '__main__':
    with open('2018/sampleinputs/day4.txt') as file:
        records = sorted([Record(r) for r in file.read().split('\n')[:-1]], key=lambda r: r.timestamp)

        sleep_log = build_sleep_log(records)

        sleeping_beauty1, log1 = sorted(sleep_log.items(), key=lambda x: x[1]['time_asleep'], reverse=True)[0]
        part1 = int(sleeping_beauty1) * int(log1['minutes'].index(max(log1['minutes'])))
        print('Part 1: {}'.format(part1))

        sleeping_beauty2, log2 = sorted(sleep_log.items(), key=lambda x: max(x[1]['minutes']), reverse=True)[0]
        part2 = int(sleeping_beauty2) * int(log2['minutes'].index(max(log2['minutes'])))
        print('Part 2: {}'.format(part2))
