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
        self.final_output = 0

def run_machine(machine_num, phase, pipeline):
    inp = pipeline.queues[machine_num].get()
    arg = "stack exec intcode ~/src/intcode/src/tests/sample_inputs/input7.txt " + str(phase) + " " + str(inp)
    out = s.Popen(arg, shell=True, stdout=s.PIPE, stderr=s.STDOUT).stdout.read()
    out = out.strip().decode()
    out = int(''.join(i for i in out if i.isdigit()))
    # the final output
    if machine_num == 4:
        pipeline.final_output=max(out, pipeline.final_output)
    # intermediate outputs are passed on
    else:
        pipeline.queues[machine_num+1].put(out)

def part1():
    num_of_machines = 5
    phase_settings = list(it.permutations(range(0,5)))
    toret = 0
    for i in phase_settings:
        pipeline = Pipeline()
        pipeline.queues[0].put(0) # initial input
        executor = cs.ThreadPoolExecutor(max_workers=5)
        for j in range(0, num_of_machines):
            executor.submit(run_machine, j, i[j], pipeline)
        executor.shutdown(wait=True)
        toret = max(toret, pipeline.final_output)
    print(toret == 17440)

def run_machine2(machine_num, phase, pipeline):
    pipeline.queues[machine_num].put(phase)
    if machine_num == 0:
        pipeline.queues[machine_num].put(0)
    next_mach = machine_num + 1 if machine_num < 4 else 0
    cmd = ["stack", "exec", "intcode", "sample_inputs/input7.txt"]
    p = s.Popen(cmd, stdout=s.PIPE, stdin=s.PIPE, stderr=s.STDOUT, text=True)
    out = 'run'
    while not out == '':
        out = p.stdout.readline().strip()
        out = str(out)
        if out == "Waiting for input...":
            inp = pipeline.queues[machine_num].get()
            inp = str(inp) + "\n"
            p.stdin.write(inp)
            p.stdin.flush()
        elif ''.join(i for i in out if i.isdigit()) == "":
            continue
        else:
            pipeline.queues[next_mach].put(out)

    p.stdin.close()
    p.terminate()

def part2():
    num_of_machines = 5
    phase_settings = list(it.permutations(range(5,10)))
    toret = 0
    for i in phase_settings:
        pipeline = Pipeline()
        executor = cs.ThreadPoolExecutor(max_workers=5)
        for j in range(0, num_of_machines):
            executor.submit(run_machine2, j, i[j], pipeline)
        executor.shutdown(wait=True)
        final_output = 0
        # I think we're probably guaranteed a single result in the queues that
        # is our final value... but the spec didn't quite say it... we'll see
        for i in pipeline.queues:
            if i.queue:
                final_output = max(final_output, int(i.get()))
        toret = max(toret, final_output)
    print(toret == 27561242)



if __name__ == "__main__":
    if len(sys.argv) == 1:
        part1()
        part2()
    elif sys.argv[1] == "1":
        part1()
    elif sys.argv[1] == "2":
        part2()
    else:
        print("Bad argument. Use '1' or '2' for part 1 or part 2, or nothing "
        "for both.")
