#!/usr/bin/python -t
# vim:tabstop=4:softtabstop=4:shiftwidth=4:expandtab

import scanf
import sys

def nice_int(i):
    if i > 1000**3:
        return str(int(i) / (1000**2)) +"b"
    if i > 1000**2:
        return str(int(i) / (1000**2)) +"m"
    if i > 1000:
        return str(int(i) / (1000)) +"k"
    return str(i)

def nice_size(i):
    if i > 2**20:
        return str(int(i) / (2**20)) +"mb"
    if i > 2**10:
        return str(int(i) / (2**10)) +"kb"
    return str(int(i)) +"b"

def round(f):
    return float(int(10.0 * float(f))) / 10.0

def nice_float(f):
    if f > 1000**3:
        return str(round(float(f) / (1000**3))) +"b"
    if f > 1000**2:
        return str(round(float(f) / (1000**2))) +"m"
    if f > 1000:
        return str(round(float(f) / (1000))) +"k"
    return str(round(float(f)))

def round_float(f):
    return str(round(f))

def parse(file):
    cmins_old = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    rmins_old = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    cmindiff  = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    rmindiff  = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    apcs_old  = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    aprs_old  = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    apcsdiff  = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                  "Reinserting": 0.0, "Antisearch": 0.0 }
    aprsdiff  = { "Inserting": 0.0, "Searching": 0.0, "Deleting": 0.0,
                      "Reinserting": 0.0, "Antisearch": 0.0 }
    for line in file:
        line = line.strip()
        if line.startswith("Size"):
            act, cnt = scanf.sscanf(line, "%s = %d")
            print "Now", nice_float(cnt), "items in BTree"
        elif line.startswith("INSERTING") or line.startswith("SEARCH") \
                or line.startswith("DELETE") or line.startswith("ANTISEARCH") \
                or line.startswith("REINSERTING"):
            try:
                act, cnt, ticks, times = scanf.sscanf(line,\
                        "%s %d %*d: %*s %*s (%d) %*s %*s (%d)")
                if act == "SEARCH":
                    act = "Searching"
                elif act == "INSERTING":
                    act = "Inserting"
                elif act == "DELETE":
                    act = "Deleting"
                elif act == "ANTISEARCH":
                    act = "Antisearch"
                elif act == "REINSERTING":
                    act = "Reinserting"
                csecs                         = ticks / 10E5
                cmins                         = csecs / 60.0
                apcs                          = cnt / csecs
                cmindiff[act]                 = cmins - cmins_old[act]
                apcsdiff[act]                 = apcs - apcs_old[act]
                cmins_old[act], apcs_old[act] = cmins, apcs
                rsecs                         = times / 10E6
                rmins                         = rsecs / 60.0
                aprs                          = cnt / rsecs
                rmindiff[act]                 = rmins - rmins_old[act]
                aprsdiff[act]                 = aprs - aprs_old[act]
                rmins_old[act], aprs_old[act] = rmins, aprs
                print act, nice_int(cnt), ": C",\
                        round_float(cmins), "min ",\
                        "("+ round_float(cmindiff[act]) +"min)  ",\
                        round_float(apcs), "ps ",\
                        "("+ round_float(apcsdiff[act]) +"ps) "
                print act, nice_int(cnt), ": R",\
                        round_float(rmins), "min ",\
                        "("+ round_float(rmindiff[act]) +"min)  ",\
                        round_float(aprs), "ps ",\
                        "("+ round_float(aprsdiff[act]) +"ps) "
            except StandardError, e:
                print line
                print "Error: ", line, e
        else:
            print line



argv = sys.argv
if len(argv) >= 2:
    del argv[0]
    for arg in argv:
        f = file(arg)
        parse(f)
else:
    f = sys.stdin
    parse(f)

