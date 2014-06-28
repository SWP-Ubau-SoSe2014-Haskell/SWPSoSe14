#!/usr/bin/env python
import sys
mode=0

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
        if i != n:
            f.write((i+3)*" "+"\\"+(3*(n-i)-3)*" ")
        if i ==0:
            f.write("  /-\\\n")
        elif i%2==1:
            f.write((i//2+1)*"   /   \\"+"\n")
        else:
            f.write("  "+"/-<"+(i//2-1)*"     --<"+"     --\\"+"\n")
    f.write((n+2+1)*" "+"\\-<"+(n//2-1)*"     --<"+"     --#\n")
    for i in range(n):
        if i%2==0:
            f.write((n+4+(i+1)*2)*" "+(n//2-i//2)*"\\   /   "+"\n")
        elif i!= n-1:
            f.write((n+3+(i+1)*2)*" "+"\\-<"+(n//2-i//2-2)*"     --<"+"     --/"+"\n")
    f.write(((3*n)+3)*" "+"\\-/")
f.close()
