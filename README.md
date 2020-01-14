# Intcode VM

This is an implementation of a machine which executes intcode, as specified by
the [2019 Advent of Code](https://adventofcode.com/2019). The machine itself is
written in (novice) Haskell. Several "tests" in Python are included; these are
the advent puzzles that used the machine, and the scripts use the machine as a
subprocess. Run them from the tests directory with the Python interpreter,
i.e., with commands like "python day7.py".

## Specification

The specification of the language and the machine was spread out over several
days, and parts of it are only available if you complete some of the puzzles. So
here is a description of the machine and the language.

An intcode program consists in a comma-separated list of integers. These are
allowed to take large values (unspecified, but at least 34463338^2). The list
consists of instructions together with parameters; each instruction-parameter
chunk is a list of integers whose length depends on the instruction.

The machine runs over the numbers, executing an instruction given its
parameters, then moving forward to the next instruction (unless the first
instruction involves jumping in the program; see below).

Therefore the machine needs to keep track of an instruction pointer, which
points to the instruction being executed. It also needs to keep track of the
relative base (see below), which is initialized to zero.

Given an instruction, the first number gives the instruction opcode and
parameter modes. The last two digits of the instruction are the opcode; the
following opcodes are available:

01: Add the next two parameters and store the result in the third parameter.

02: Multiply the next two parameters and store the result in the third
parameter.

03: Input a value and store it in the first parameter.

04: Output the value in the first parameter.

05: Jump to the address in the second parameter if the first parameter is not
zero.

06: Jump to the address in the second parameter if the first parameter is zero.

07: Compare the first parameter to the second parameter; if the first is less
than the second, store a 1 in the third parameter, otherwise 0.

08: Compare the first parameter to the second parameter; if the first is equal
to the second, store a 1 in the third parameter, otherwise 0.

09: Modify the relative base by adding the first parameter to it.

99: Halt.

The program itself lies at the beginning of memory. Note that the program can be
modified as it is executed. Usable memory extends beyond the program and is
initialized to zero. It is an error to attempt to access memory at a negative
address, but the program may contain negative numbers.

A parameter can be used in multiple ways:

- Position mode (0): the parameter is to be interpreted as an address in the
  machine's memory.

- Immediate mode (1): the parameter is interpreted directly as a number.

- Relative mode (2): the parameter is interpreted as the address which is the
  value of the parameter added to the current relative base.

When the parameter is read from in position or relative mode, the value at the
address is used as an argument to the opcode function. When a value is stored in
a parameter, memory at that address is replaced with that value. It's impossible
to write to a parameter in immediate mode, and a valid program will never ask
you to do so.

The parameter modes are indicated in order by the digits of the instruction
prior to the opcode, from least to most significant. Pad the instruction with
leading zeros if it is too short to yield digits for every parameter (i.e.,
default to position mode).

For example, the instruction 1001 tells the machine to add the number after the
instruction to the number stored at the address two after the instruction, and
to store the result at the address three after the instruction.

This implementation uses Haskell Int as its word, and so on a 64-bit
architecture can handle large numbers. If you want to use large enough numbers
to pass the tests with a 32-bit architecture, you'll have to adjust IWord to be
something larger than an Int and use a Map instead of an IntMap as the Program
type (I did not test, but this should incur a performance hit).

The machine is invoked at the command-line and I/O is handled through stdin and
stdout; the machine will block when waiting for input. In general the
implementation assumes a valid intcode program and may not react gracefully if
it is not given one.
