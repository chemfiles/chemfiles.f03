#!/usr/bin/env python
# coding=utf-8
"""
This script check that all the interface functions (as defined in
"src/generated/types.f90") are documented.
"""
from __future__ import unicode_literals, absolute_import
import sys
import os
from codecs import open

from functions import all_functions

ERROR = False
ROOT = os.path.join(os.path.dirname(__file__), "..")


def error(message):
    print(message)
    global ERROR
    ERROR = True


def usage_in_doc():
    usages = []
    for (root, _, pathes) in os.walk(os.path.join(ROOT, "doc", "reference")):
        for path in pathes:
            with open(os.path.join(root, path), encoding="utf8") as fd:
                for line in fd:
                    if line.startswith(".."):
                        _, kind = line.split()[:2]
                        if kind in ["f:function::", "f:subroutine::"]:
                            name = line.split()[2].split('(')[0]
                            usages.append(name)
    return usages


def type_bound_functions():
    functions = []
    for (root, _, pathes) in os.walk(os.path.join(ROOT, "doc", "reference")):
        for path in pathes:
            with open(os.path.join(root, path), encoding="utf8") as fd:
                type = None
                for line in fd:
                    if line.startswith(".."):
                        _, kind = line.split()[:2]
                        if kind == "f:type::":
                            type = line.split()[2]

                    if type and ":field subroutine" in line:
                        name = line.split()[2][:-1]
                        functions.append(type + "%" + name)
    return functions


if __name__ == '__main__':
    functions = all_functions()
    # We also document the 'chfl_warning_callback' interface as a function
    functions.append("chfl_warning_callback")
    docs = usage_in_doc()
    for function in functions:
        if function not in docs:
            error("missing documentation for {}".format(function))
    for function in docs:
        if function not in functions:
            error("documentation for non-existing {}".format(function))

    types_bound = type_bound_functions()
    for function in types_bound:
        if function not in docs:
            error("mismatch in type: missing doc for {}".format(function))

    for function in docs:
        if function not in types_bound and '%' in function:
            error("mismatch in type: missing field for {}".format(function))

    if ERROR:
        sys.exit(1)
