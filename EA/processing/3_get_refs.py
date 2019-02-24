""" Parse the string-based citations from EA """

with open('references.txt') as file:
    for line in file:
        line = line.strip()
        tokens = line.split('.', 1)
        print("\t".join(tokens))
