#!/usr/bin/env python
import sys

n = int(sys.argv[1])
f = open("perftest.rail","w")
f.write("$ 'main'\n")

for i in range(n):
    f.write(" \\-")
    f.write(n*"1o")
    f.write("-\\\n")
    f.write((4+n*2)*" "+ "|\n")
    f.write(" /-"+n*("o1")+"-/\n")
    f.write("|\n")

f.write("#")
f.close()
