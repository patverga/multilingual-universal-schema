__author__ = 'pat'
from collections import defaultdict
from random import shuffle, randint

pair_lines = defaultdict(list)

# read lines into a map from mention pair -> lines containing that pair
for line in open('english.arvind.utf.sorted.min2', 'r'):
    parts = line.split('\t')
    pair = parts[0]
    pair_lines[pair].append(line)

# train test dev
out_names = ["train", "dev", "test"]
splits = [[], [], []]
last_pair = ""
for (mention_pair, lines) in pair_lines.iteritems():
    # shuffle the lines from the mention pair
    shuffle(lines)
    for (i, line) in enumerate(lines):
        # always add the first random line to test
        index = 0 if i == 0 else randint(0, 2)
        splits[index].append(line)

for (split, name) in zip(splits, out_names):
    shuffle(split)
    f = open(name, 'w')
    for line in split:
        f.write(line)
    f.close()