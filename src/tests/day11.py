# script to solve day 11 problem. uses intcode vm to print a picture

from collections import defaultdict
import itertools as it
import matplotlib.pyplot as plt
import numpy as n
import subprocess as s
import sys



# todo: replace with logging module
debug = False
def noop(*args, **kargs):
    pass

if debug:
    dprint = print
else:
    dprint = noop

class Robot:
    dirs = [(0,1), (1,0), (0, -1), (-1, 0)]

    def __init__(self):
        self.x_cur, self.y_cur = (0, 0)
        self.facing = 0 # idx into dirs
        self.p_locs = defaultdict(lambda: defaultdict(int))

    def rotate(self, i): # i is 0 or 1
        if i:
            self.facing = (self.facing + 1) % 4
        else:
            self.facing = (self.facing - 1) % 4

    def paint_cur_loc(self, i): # 0 or 1
        if i:
            self.p_locs[self.x_cur][self.y_cur] = 1
        else:
            self.p_locs[self.x_cur][self.y_cur] = 0

    def move(self):
        self.x_cur, self.y_cur = (self.x_cur + Robot.dirs[self.facing][0], self.y_cur + Robot.dirs[self.facing][1])

    def print_painting(self):
        coordinates = []
        for i, j in self.p_locs.items():
            for k, v in j.items():
                if v == 1:
                    coordinates.append((i, k))
        dprint(coordinates)
        xs = [c[0] for c in coordinates]
        ys = [c[1] for c in coordinates]
        plt.scatter(xs, ys, marker='s') # this turns out to be kind of hard to
        plt.show()                      # read but it did work


    def run_robot(self, part):
        if part == 2:
            self.p_locs[0][0] = 1
        cmd = ["stack", "exec", "intcode", "/home/umer/src/intcode/src/tests/sample_inputs/input11.txt"]
        p = s.Popen(cmd, stdout=s.PIPE, stdin=s.PIPE, stderr=s.PIPE, text=True)
        out = "dummy_val"

        while not out == "":
            out = p.stdout.readline().strip() # "waiting for input"
            if out == "Done":
                break
            dprint("intcode output 1: ", out)
            color = (self.p_locs[self.x_cur][self.y_cur])
            dprint("giving intcode vm color at", self.x_cur, self.y_cur, ": ", color)
            dprint(str(color) + "\n")
            p.stdin.write(str(self.p_locs[self.x_cur][self.y_cur]) + "\n")
            p.stdin.flush()

            out = p.stdout.readline().strip() # first number
            out = int(out)
            dprint("ivm output1:", out)
            self.paint_cur_loc(out)
            color = (self.p_locs[self.x_cur][self.y_cur])
            dprint("new color:" , color)
            out = int(p.stdout.readline().strip())
            dprint("ivm output2:", out)
            self.rotate(out)
            self.move()
            dprint("new location:", self.x_cur, self.y_cur)
        dprint("halted")
        num_painted_locs = sum(list(map(len,[v for v in self.p_locs.values()])))
        if part == 1:
            print(num_painted_locs == 2418)
        if part == 2:
            self.print_painting()

def main(i):
    if i == 0:
        painter = Robot()
        painter.run_robot(1)
        painter = Robot() # reset
        painter.run_robot(2)
    else:
        painter = Robot()
        painter.run_robot(i)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        main(0)
    elif sys.argv[1] == "1":
        main(1)
    elif sys.argv[1] == "2":
        main(2)
    else:
        print("Bad argument. Use '1' or '2' for part 1 or part 2, or nothing "
        "for both.")
