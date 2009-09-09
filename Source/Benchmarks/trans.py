#!/usr/bin/python -t
# vim:tabstop=4:softtabstop=4:shiftwidth=4:expandtab

import sys

input = sys.stdin
lists = []

for line in input:
    tokens = line.strip().split(" ")
    while len(lists) < len(tokens):
        lists.append([])
    for token in tokens:
        list = lists[tokens.index(token)]
        list.append(token)

for list in lists:
    for item in list:
        print item, 
    print ""

