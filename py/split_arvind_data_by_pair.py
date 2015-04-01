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

train_relations_set = set([line.split('\t')[1] for line in splits[0]])
for i in range(1, 3):
    not_in_train = {line.split('\t')[1]: line for line in splits[i] if line.split('\t')[1] not in train_relations_set}
    train_relations_set.update(not_in_train.keys())
    not_in_train_set = set(not_in_train.values())
    splits[i] = [line for line in splits[i] if line not in not_in_train_set]
    splits[0] += [line for line in splits[i] if line in not_in_train_set]

print(len(splits[0]), len(splits[1]), len(splits[2]))

for (split, name) in zip(splits, out_names):
    shuffle(split)
    f = open(name, 'w')
    for line in split:
        f.write(line)
    f.close()