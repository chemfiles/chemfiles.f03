#!/usr/bin/env python
# coding=utf-8
"""
This script check that all the functions are effectivelly tested. It does so by
reading the tests and parsing declarations to get the variables types.

This script is not robust, and can not parse all fortran declarations, but
should be good enough for our usage.
"""
import sys
import os
import re

from functions import all_functions

ERROR = False
ROOT = os.path.join(os.path.dirname(__file__), "..")


VARIABLE_DECLARATION = re.compile("type\((.*?)\) :: (.*)")
TYPE_BOUND_SUBROUTINE = re.compile("call (.*?)\%(.*?)\(.*status=status\)")
FREE_SUBROUTINE = re.compile("call (.*?)\%(free|close)\(\)")
TYPE_BOUND_FUNCTION = re.compile("[\s\(](.*?)\%(.*?)\(.*status=status\)")
OTHER_FUNCTIONS = re.compile("[=(call)]\s+chfl_(.*?)\(.*\)")


def error(message):
    print(message)
    global ERROR
    ERROR = True


def usage_in_tests():
    usages = set()
    for (root, _, paths) in os.walk(os.path.join(ROOT, "tests")):
        for path in paths:
            variables = {}
            with open(os.path.join(root, path)) as fd:
                for line in fd:
                    if "end subroutine" in line:
                        # reset variables
                        variables = {}

                    if "!" in line:
                        continue

                    # Get variables types from declarations
                    match = VARIABLE_DECLARATION.search(line)
                    if match:
                        chfl_type, vars = match.groups()
                        if chfl_type.startswith("chfl_match"):
                            continue
                        for var in vars.split(","):
                            variables[var.strip()] = chfl_type
                        continue

                    # type bound subroutines
                    match = TYPE_BOUND_SUBROUTINE.search(line)
                    if match:
                        var, function = match.groups()
                        chfl_type = variables[var]
                        usages.add(chfl_type + "%" + function)
                        continue

                    # type bound functions
                    match = TYPE_BOUND_FUNCTION.search(line)
                    if match:
                        var, function = match.groups()
                        var = var.split("(")[-1]
                        var = var.split()[-1]
                        if var in variables:
                            chfl_type = variables[var]
                            usages.add(chfl_type + "%" + function)
                            continue

                    match = FREE_SUBROUTINE.search(line)
                    if match:
                        var, function = match.groups()
                        var = var.split("(")[-1]
                        var = var.split()[-1]
                        if var in variables:
                            chfl_type = variables[var]
                            usages.add(chfl_type + "%" + function)
                            continue

                    # other functions/subroutines
                    match = OTHER_FUNCTIONS.search(line)
                    if match:
                        name = match.groups()[0]
                        usages.add("chfl_" + name)
                        continue

    return usages


if __name__ == '__main__':
    functions = all_functions()
    tests = usage_in_tests()
    for function in functions:
        if function not in tests:
            error("missing tests for {}".format(function))

    if ERROR:
        sys.exit(1)
