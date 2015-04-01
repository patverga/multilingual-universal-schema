__author__ = 'pat'
from collections import defaultdict
from random import shuffle, randint

relation_lines = defaultdict(list)

# read lines into a map from relation -> lines containing that relation
for line in open('english.arvind.utf.sorted.min2', 'r'):
    parts = line.split('\t')
    relation = parts[1]
    relation_lines[relation].append(line)

# train test dev
out_names = ["train", "dev", "test"]
splits = [[], [], []]
last_pair = ""
for (relation, lines) in relation_lines.iteritems():
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