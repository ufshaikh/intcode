# script to solve day 7 problem. daisy-chains intcode machines

import itertools as it
import subprocess as s
import sys
import queue
import concurrent.futures as cs

class Pipeline:
    """Object to pass intermediate results back and forth between amplifiers."""
    def __init__(self):
        self.queues = list(map(queue.Queue, range(0,5)))
        self.queues[0].put("0") # initial input
        self.final_output = 0


def part1():
    def run_machine(machine_num, phase, pipeline):
        inp = pipeline.queues[machine_num].get()
        arg = "stack exec intcode ~/src/intcode/src/tests/sample_inputs/input7.txt " + str(phase) + " " + inp
        out = s.Popen(arg, shell=True, stdout=s.PIPE).stdout.read()
        out = out.strip().decode()
        # the final output
        if machine_num == 4:
            pipeline.final_output=max(int(out), pipeline.final_output)
        # intermediate outputs are passed on
        else:
            pipeline.queues[machine_num+1].put(out)

    num_of_machines = 5
    phase_settings = list(it.permutations(range(0,5)))
    toret = 0
    for i in phase_settings:
        pipeline = Pipeline()
        executor = cs.ThreadPoolExecutor(max_workers=5)
        fut_objs = it.repeat(0, num_of_machines)
        for j in range(0, num_of_machines):
            executor.submit(run_machine, j, i[j], pipeline)
        executor.shutdown(wait=True)
        toret = max(toret, pipeline.final_output)
    print(toret == 17440)

if __name__ == "__main__":
    if sys.argv[1] == "1":
        part1()
    elif sys.argv[1] == "2":
        part2()
    else:
        part1()
        part2()
