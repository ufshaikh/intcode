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
consists of instructions together with parameters, each of which is a list of
integers whose length varies depending on the instruction.

The machine runs over the numbers, executing an instruction given its
parameters, then moving forward to the next instruction (unless the first
instruction involves jumping in the program; see below).

Therefore the machine needs to keep track of an instruction pointer, which
points to the instruction being executed. It also needs to keep track of the
relative base (see below), which is initialized to zero.

Given an instruction, the first number gives the instruction opcode and
parameter modes. The last two digits of the instruction are the opcode; the
following opcodes are available:

1: Add the next two parameters and store the result in the third parameter.

2: Multiply the next two parameters and store the result in the third parameter.

3: Input a value and store it in the first parameter.

4: Output the value in the first parameter.

5: Jump to the address in the second parameter if the first parameter is not
zero.

6: Jump to the address in the second parameter if the first parameter is zero.

7: Compare the first parameter to the second parameter; if the first is less
than the second, store a 1 in the third parameter, otherwise 0.

8: Compare the first parameter to the second parameter; if the first is equal to
the second, store a 1 in the third parameter, otherwise 0.

9: Modify the relative base by adding the first parameter to it.

99: Halt.

The program itself lies at the beginning of memory. Note that the program can be
modified as it is executed. Usable memory extends beyond the program and is
initialized to zero. It is an error to attempt to access memory at a negative
address, but the program may contain negative numbers.

A parameter can be used in multiple ways:

- Position mode (0): the parameter is to be interpreted as an address in the
  machine's "memory."

- Immediate mode (1): the parameter is interpreted as a number.

- Relative mode (2): the parameter is interpreted as an offset from the current
  relative base.

It's impossible to write to a parameter in immediate mode, and a valid program
will never ask you to do so.

The parameter modes are indicated in order by the digits of the instruction
prior to the opcode, from least to most significant. Pad the instruction with
leading zeros if it is too short to yield digits for every parameter (i.e.,
default to position mode).

For example, the instruction 1001 tells the machine to add the number after the
instruction to the number stored at the address two after the instruction, and
to store the result at the address three after the instruction.

This implementation uses Haskell Int as its word, and so on a 64-bit
architecture can handle large numbers. The machine is invoked at the
command-line and I/O is handled through stdin and stdout; the machine will block
when waiting for input. In general the implementation assumes a valid intcode
program and may not react gracefully if it is not given one.
