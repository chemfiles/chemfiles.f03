# coding=utf-8
"""
Getting all the public functions/constants exported from the fortran API
"""
import os
import glob

ROOT = os.path.join(os.path.dirname(__file__), "..")


def all_functions():
    functions = []
    type = None
    for path in glob.glob(os.path.join(ROOT, "src", "*.f90")):
        with open(path) as fd:
            for line in fd:
                if "end type" in line:
                    type = None
                    break
                if line.strip().startswith("type chfl_"):
                    type = line.split()[1]
                    continue
                if type and "procedure" in line:
                    if "private" in line:
                        continue
                    procedures = line.split("::")[1].split(",")
                    for procedure in procedures:
                        procedure = procedure.split("=>")[0]
                        procedure = procedure.strip()
                        if procedure.startswith("unsafe_"):
                            continue
                        functions.append(type + "%" + procedure)
                if type and "generic" in line:
                    procedures = line.split("::")[1]
                    procedure = procedures.split(",")[0].split("=>")[0]
                    procedure = procedure.strip()
                    functions.append(type + "%" + procedure)

    with open(os.path.join(ROOT, "src", "chemfiles.f90")) as fd:
        for line in fd:
            if "function" in line or "subroutine" in line:
                if 'end' in line or "!" in line:
                    continue
                name = line.split()[1].split('(')[0]
                if "internal" in name:
                    continue
                if name == 'chfl_warning_callback':
                    continue
                functions.append(name)

    return functions
