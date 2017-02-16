# coding=utf-8
"""
Getting all the public functions/constants exported from the fortran API
"""
import os

ROOT = os.path.join(os.path.dirname(__file__), "..")


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

    others = os.path.join(ROOT, "src", "generated", "wrapper", "others.f90")
    with open(others) as fd:
        for line in fd:
            if line.startswith("function") or line.startswith("subroutine"):
                name = line.split()[1].split('(')[0]
                functions.append(name)
    functions.append("chfl_set_warning_callback")
    return functions
