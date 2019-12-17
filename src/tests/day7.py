# script to solve day 7 problem. daisy-chains intcode machines

import concurrent.futures as cs
import itertools as it
import subprocess as s
import sys
import queue

class Pipeline:
    """Object to pass intermediate results back and forth between amplifiers."""
    def __init__(self):
        self.queues = list(map(queue.Queue, range(0,5)))
        self.final_output = 0

def run_amplifier(amp_num, phase, pipeline):
    pipeline.queues[amp_num].put(phase)
    if amp_num == 0: # initial value kicks off the whole process
        pipeline.queues[amp_num].put(0)
    next_amp = (amp_num + 1) % 5
    cmd = ["stack", "exec", "intcode", "sample_inputs/input7.txt"]
    p = s.Popen(cmd, stdout=s.PIPE, stdin=s.PIPE, stderr=s.STDOUT, text=True)
    out = 'dummy_val'

    while not out == '':
        out = p.stdout.readline().strip()
        if out == "Waiting for input...":
            inp = pipeline.queues[amp_num].get()
            inp = str(inp) + "\n"
            p.stdin.write(inp)
            p.stdin.flush()
        # for the ending message "Done"
        elif ''.join(i for i in out if i.isdigit()) == "":
            continue
        else:
            pipeline.queues[next_amp].put(out)

    p.stdin.close()
    p.terminate()

def set_up_amplifier_pool(part):
    num_of_amps = 5
    if part == 1:
        phase_settings = list(it.permutations(range(0,5)))
    else: # part == 2:
        phase_settings = list(it.permutations(range(5,10)))
    toret = 0

    for i in phase_settings:
        pipeline = Pipeline()
        executor = cs.ThreadPoolExecutor(max_workers=5)
        for j in range(0, num_of_amps):
            executor.submit(run_amplifier, j, i[j], pipeline)
        executor.shutdown(wait=True)
        final_output = 0
        # I think we're probably guaranteed a single result in the queues that
        # is our final value... but the spec didn't quite say it... we'll see
        for i in pipeline.queues:
            if i.queue:
                final_output = max(final_output, int(i.get()))
        toret = max(toret, final_output)

    if part == 1:
        print(toret == 17440)
    else: # part 2
        print(toret == 27561242)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        set_up_amplifier_pool(1)
        set_up_amplifier_pool(2)
    elif sys.argv[1] == "1":
        set_up_amplifier_pool(1)
    elif sys.argv[1] == "2":
        set_up_amplifier_pool(2)
    else:
        print("Bad argument. Use '1' or '2' for part 1 or part 2, or nothing "
        "for both.")
