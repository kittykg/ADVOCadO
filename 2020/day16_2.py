import numpy as np


def read_rule(ruleStr):
    tokens = ruleStr.split(': ')
    ranges = list(map(lambda x: x.split('-'),
                      tokens[1].split(' or ')))
    parts = list(map(lambda x: list(map(int, x)), ranges))
    valid = set(range(parts[0][0], parts[0][1] + 1))
    valid.update(range(parts[1][0], parts[1][1] + 1))
    return tokens[0], valid


def read():
    with open('input_16', 'r') as f:
        parts = list(map(lambda x: x.split('\n'), f.read().split('\n\n')))
        rules, mine, nearby = parts
        return \
            {read_rule(s)[0]: read_rule(s)[1] for s in rules}, \
            list(map(int, mine[1].split(','))), \
            list(map(lambda x: list(map(int, x.split(','))), nearby[1:-1]))


def all_possible(rules):
    possible = set()
    for r in rules.values():
        possible.update(r)
    return possible


def is_valid(ap, ticket):
    return set(ticket).issubset(ap)


def part_2():
    rules, mine, nearby = read()
    ap = all_possible(rules)
    nb_t = np.array(list(filter(lambda t: is_valid(ap, t), nearby))).T
    available = set(rules.keys())
    choices = dict()
    for i, nb in enumerate(nb_t):
        temp = available.copy()
        for a in available:
            if not set(nb).issubset(rules[a]):
                temp.remove(a)
        choices[i] = temp

    config = dict()
    while len(available) > 0:
        for i in choices.keys():
            if i in config:
                continue

            if len(choices[i]) == 1:
                field = choices[i].pop()
                available.remove(field)
                config[i] = field

            if not choices[i].issubset(available):
                choices[i] = choices[i].intersection(available)

    prod = 1
    for k, v in config.items():
        if v.startswith('departure'):
            prod *= mine[k]

    print(prod)


if __name__ == '__main__':
    part_2()
