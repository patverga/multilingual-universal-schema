__author__ = 'pat'
from random import shuffle, randint

out_names = ["train", "dev", "test"]
splits = [[], [], []]
pair_set = set()
relation_set = set()

# read lines into a map from mention pair -> lines containing that pair
for line in open('english.arvind.utf.sorted.min2', 'r'):
    parts = line.split('\t')
    pair = parts[0]
    relation = parts[1]
    index = 0 if pair not in pair_set or relation not in relation_set else randint(0, 2)
    splits[index].append(line)
    if pair not in pair_set:
        pair_set.add(pair)
    if relation not in relation_set:
        relation_set.add(relation)

print(len(splits[0]), len(splits[1]), len(splits[2]))

for (split, name) in zip(splits, out_names):
    shuffle(split)
    f = open(name, 'w')
    for line in split:
        f.write(line)
    f.close()