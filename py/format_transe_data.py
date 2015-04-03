__author__ = 'pat'
from random import shuffle, randint
import sys


def get_id(id_dict, output_file, part):
    if part in id_dict:
        part_id = id_dict[part]
    else:
        part_id = len(id_dict)
        id_dict[part] = part_id
        # update the id map
        output_file.write(part.strip() + ' ' + str(part_id) + '\n')
    return part_id


def main(argv):
    relation_dict = {}
    entity_dict = {}

    relation_file = open("relation2id.txt", 'w')
    entity_file = open("entity2id.txt", 'w')
    data_file = open('data.txt', 'w')

    for line in open(argv[0], 'r'):
        parts = line.split('\t')
        id_0 = get_id(entity_dict, entity_file, parts[0])
        id_1 = get_id(relation_dict, relation_file, parts[1])
        id_2 = get_id(entity_dict, entity_file, parts[2])

        # update the actual file
        out_line = str(id_0) + ' ' + str(id_2) + ' ' + str(id_1) + '\n'
        data_file.write(out_line)

    relation_file.close()
    entity_file.close()
    data_file.close()


if __name__ == "__main__":
    main(sys.argv[1:])