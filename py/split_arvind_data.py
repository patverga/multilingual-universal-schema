__author__ = 'pat'
from random import shuffle, randint
import sys


def main(argv):
    if len(argv) < 1:
        print ("must supply input file")
        sys.exit()

    neg_samples = 10
    out_names = ["train", "dev", "test"]
    splits = [[], [], []]
    pair_set = set()
    relation_set = set()

    # save all pairs first to negative sample
    pairs = []
    for line in open(argv[0], 'r'):
        pairs.append(line.split('\t')[0])
    shuffle(pairs)

    for line in open(argv[0], 'r'):
        parts = line.split('\t')
        pair = parts[0]
        relation = parts[1]
        index = 0 if pair not in pair_set or relation not in relation_set else randint(0, 2)
        splits[index].append(line)
        # add negative samples
        if index > 0:
            for i in range(1, neg_samples):
                splits[randint(1, 2)].append(pairs[randint(0, len(pairs) - 1)] + "\t" + relation + "\t0\n")

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


if __name__ == "__main__":
    main(sys.argv[1:])