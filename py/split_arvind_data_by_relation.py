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

# need to make sure train set also contains each pair
train_pairs_set = set([line.split('\t')[0] for line in splits[0]])
for i in range(1, 2):
    splits[i] = [line for line in splits[i] if line.split('\t')[0] not in train_pairs_set]
    splits[0] += [line for line in splits[i] if line.split('\t')[0] in train_pairs_set]

print(len(splits[0]), len(splits[1]), len(splits[2]))

for (split, name) in zip(splits, out_names):
    shuffle(split)
    f = open(name, 'w')
    for line in split:
        f.write(line)
    f.close()