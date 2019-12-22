from collections import defaultdict
import subprocess as s

cmd = ["stack", "exec", "intcode", "/home/umer/src/intcode/src/tests/sample_inputs/input13.txt"]
p = s.Popen(cmd, stdout=s.PIPE, stdin=s.PIPE, stderr=s.PIPE, text=True)
instructions = list(map(int, p.stdout.read().split("\n")[:-2])) # -2 for 'Done' and ending \n

screen = defaultdict(lambda: defaultdict(int)) # y, x, tile id
i = 0
while i < len(instructions):
    screen[i+1][i] = instructions[i+2]
    i += 3

toret = 0
for i in screen.values():
    for j in i.values():
        if j==2:
           toret+=1
print(toret)
