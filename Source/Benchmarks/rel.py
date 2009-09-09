#!/usr/bin/python -t
# vim:tabstop=4:softtabstop=4:shiftwidth=4:expandtab

import scanf
import sys

def print_stats(last_abs_cpu, last_abs_real, count):
    avg_cpu  = 1000.0 * last_abs_cpu  / count
    avg_real = 1000.0 * last_abs_real / count
    print str(count / 1000) +"k ",\
            "average (millis): ",\
            str(avg_cpu),\
            " ",\
            str(avg_real)


def parse(file):
    count         = 1
    last_abs_cpu  = 0
    last_abs_real = 0
    printed_last  = 1
    for line in file:
        line = line.strip()
        try:
            tuple = scanf.sscanf(line, "%d: %f %f")
            count, abs_cpu, abs_real = tuple

            if abs_cpu > last_abs_cpu:
                rel_cpu      = abs_cpu - last_abs_cpu
                last_abs_cpu = abs_cpu
            else:
                rel_cpu      = abs_cpu

            if abs_real > last_abs_real:
                rel_real      = abs_real - last_abs_real
                last_abs_real = abs_real
            else:
                rel_real      = abs_real
            printed_last = 0
            print str(count / 1000)+"k: ", str(rel_cpu), " ", str(rel_real)
        except:
            if line.endswith("."):
                if not printed_last:
                    print_stats(last_abs_cpu, last_abs_real, count)
                    last_abs_cpu  = 0
                    last_abs_real = 0
                    printed_last = 1
                print line
        #except StandardError, e:
            #print "Error: ", line, e
    if not printed_last:
        print_stats(last_abs_cpu, last_abs_real, count)
        last_abs_cpu  = 0
        last_abs_real = 0



argv = sys.argv
if len(argv) >= 2:
    del argv[0]
    for arg in argv:
        f = file(arg)
        parse(f)
else:
    f = sys.stdin
    parse(f)

