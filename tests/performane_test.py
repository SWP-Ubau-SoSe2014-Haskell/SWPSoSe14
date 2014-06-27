#!/usr/bin/env python
import sys
mode=1

n = int(sys.argv[1])
f = open("perftest.rail","w")
f.write("$ 'main'\n")

if mode==0:
    for i in range(n):
        f.write(" \\-")
        f.write(n*"1o")
        f.write("-\\\n")
        f.write((4+n*2)*" "+ "|\n")
        f.write(" /-"+n*("o1")+"-/\n")
        f.write("|\n")
    f.write("#")
elif mode==1:
    n = 2 * n
    f.write(" \\\n")
    f.write("  \\\n")
    for i in range(n):
        f.write((i+3)*" "+"\\\n")

f.close()
