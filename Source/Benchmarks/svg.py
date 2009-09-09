#!/usr/bin/python -t
# vim:tabstop=4:softtabstop=4:shiftwidth=4:expandtab

import sys

width = 1024
height = 768
colors = {  "Count"           : "black",\
            "Height"          : "darkgreen",\
            "Blocks"          : "brown",\
            "UsedBlocks"      : "orange",\
            "FreeBlocks"      : "cyan",\
            "MaxDegree"       : "darkgray",\
            "AvgDegree"       : "lightgray",\
            "MinDegree"       : "lightgray",\
            "TotalWaste"      : "blue",\
            "TotalSizes"      : "green",\
            "WastePerBlock"   : "darkred",\
            "RelWastePerBlock": "red" }
headers = colors.keys()

def maximum(list):
    max = 0
    for item in list:
        if item > max:
            max = item
    return max

def minimum(list):
    max = 0
    for item in list:
        if item < max:
            max = item
    return max

def to_floats(list):
    tmp = []
    for item in list:
        tmp.append(float(item))
    return tmp

def px(x):
    return int(x * width)

def py(y):
    return int((1 - y) * height)

polys = []
input = sys.stdin
for line in input:
    tokens = line.strip().split(" ")
    if len(tokens) == 0 or tokens[0] == "OK":
        continue
    tokens = to_floats(tokens)
    max = maximum(tokens)
    min = 0
    cnt = len(tokens)
    poly = []
    for i in range(0, cnt-1):
        x = float(i) / (cnt-1)
        y = float(tokens[i]) / max
        poly.append((x,y))
    polys.append(poly)


print """
<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 
'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>
<svg width='100%' height='100%' version='1.1'
xmlns='http://www.w3.org/2000/svg'>
"""

for poly in polys:
    header = headers[polys.index(poly)]
    color = colors[header]
    print "<polyline points=\"",
    for (x, y) in poly:
        print str(px(x)) +","+ str(py(y)) +" ",
    print "\" style=\"stroke:#"+color+";stroke-width:2;fill:none\"/>"
    x = str(px(float(polys.index(poly)) / len(polys)))
    y = str(py(0.2))
    print "<text x='"+x+"' y='"+y+"' style='fill:"+color+"'>"+header+"</text>"


print """
</svg>
"""
