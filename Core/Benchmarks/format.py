#!/usr/bin/python -t
# vim:tabstop=4:softtabstop=4:shiftwidth=4:expandtab

import sys

headers = [ "",\
        "Count", "Height", "Blocks", "UsedBlocks", "FreeBlocks", "MaxDegree",\
        "AvgDegree", "MinDegree", "TotalWaste", "TotalSizes", "WastePerBlock",\
        "RelWastePerBlock" ]

def nice_header(h):
    maxlen = 0
    for header in headers:
        if len(header) > maxlen:
            maxlen = len(header)
    difflen = maxlen - len(h)
    s = h +": "
    for i in range(1, difflen):
        s += " "
    return s

def nice_int(i):
    if (i / (1000**2)) > 0:
        return str(i / (1000**2)) +"m"
    if (i / (1000)) > 0:
        return str(i / (1000)) +"k"
    return str(i)

def nice_size(i):
    if (i / (2**20)) > 0:
        return str(i / (2**20)) +"mb"
    if (i / (2**10)) > 0:
        return str(i / (2**10)) +"kb"
    return str(i) +"b"

def nice_float(f):
    return str(f)

input = sys.stdin
header_iter = iter(headers)
for line in input:
    tokens = line.strip().split(" ")
    header = header_iter.next()
    if len(header) == 0:
        continue
    print nice_header(header), 
    for token in tokens:
        if header in set(["Count", "Blocks", "UsedBlocks", "FreeBlocks"]):
            s = nice_int(int(token))
        if header in set(["TotalWaste", "TotalSizes", "WastePerBlock"]):
            s = nice_size(int(token))
        if header == "RelWastePerBlock":
            s = nice_float(float(token))
        print s,
    print

