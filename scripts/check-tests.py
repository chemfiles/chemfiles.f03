#!/usr/bin/env python
# coding=utf-8
"""
This script check that all the interface functions (as defined in
"src/generated/types.f90") are effectivelly tested. It does so by reading the
tests and parsing declarations to get the variables types.

This script is not robust, and can not parse all fortran declarations, but
should be good enough for our usage.
"""
import sys
import os
import re

ERROR = False
ROOT = os.path.join(os.path.dirname(__file__), "..")


def error(message):
    print(message)
    global ERROR
    ERROR = True


def all_functions():
    functions = []
    current_type = None
    with open(os.path.join(ROOT, "src", "generated", "types.f90")) as fd:
        for line in fd:
            if "end type" in line:
                current_type = None
                continue
            if line.startswith("type"):
                current_type = line.split()[1]
                continue
            if current_type and "procedure" in line:
                procedure = line.split()[2]
                functions.append(current_type + "%" + procedure)
    return functions


def usage_in_tests():
    usages = []
    for (root, _, paths) in os.walk(os.path.join(ROOT, "tests")):
        for path in paths:
            variables = {}
            with open(os.path.join(root, path)) as fd:
                for line in fd:
                    # Get variables types from declarations
                    match = re.search("type\((.*?)\) :: (.*)", line)
                    if match:
                        chfl_type, vars = match.groups()
                        if chfl_type.startswith("chfl_match"):
                            continue
                        for var in vars.split(","):
                            variables[var.strip()] = chfl_type

                    if "call" in line and "call check" not in line:
                        match = re.search("call (.*?)\%(.*?)\(.*\)", line)
                        if match:
                            var, function = match.groups()
                            chfl_type = variables[var]
                            usages.append(chfl_type + "%" + function)
    return usages


if __name__ == '__main__':
    functions = all_functions()
    tests = usage_in_tests()
    for function in functions:
        if function not in tests:
            error("missing tests for {}".format(function))

    if error:
        sys.exit(1)
