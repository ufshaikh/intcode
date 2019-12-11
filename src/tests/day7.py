# script to solve day 7 problem. daisy-chains intcode machines

import itertools as iter
import subprocess as s
import sys

def part1():
    phase_settings = list(iter.permutations(range(0,5)))

    toret = 0
    for i in phase_settings:
        inp = 0
        for j in i:
            com_str = "stack exec intcode sample_inputs/input7.txt " + str(j) + " " + str(inp)
            inp = s.Popen(com_str, shell=True, stdout=s.PIPE).stdout.read()
            inp = int(inp.strip().decode())
        toret = max(toret, inp)

    print(toret == 17440)

def part2():
    print("testing")


if __name__ == "__main__":
    if sys.argv[1] == "1":
        part1()
    elif sys.argv[1] == "2":
        part2()
    else:
        print("Bad argument, use '1' or '2' for part 1 or part 2.")
