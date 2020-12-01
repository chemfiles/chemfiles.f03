# coding=utf-8
"""
Getting all the public functions/constants exported from the fortran API
"""
import os
import glob

ROOT = os.path.join(os.path.dirname(__file__), "..")


def all_functions():
    functions = []
    current_type = None
    for path in glob.glob(os.path.join(ROOT, "src", "*.f90")):
        with open(path) as fd:
            for line in fd:
                if "end type" in line:
                    current_type = None
                    break
                if line.strip().startswith("type, extends(chfl_ptr)"):
                    current_type = line.split()[3]
                    if current_type != "chfl_trajectory":
                        functions.append(current_type + "%free")
                    continue
                if current_type:
                    if "private" in line:
                        continue

                    if "procedure" in line:
                        procedures = line.split("::")[1].split(",")
                        for procedure in procedures:
                            procedure = procedure.split("=>")[0]
                            procedure = procedure.strip()
                            if procedure.startswith("unsafe_"):
                                continue
                            functions.append(current_type + "%" + procedure)

                    if "generic" in line:
                        procedures = line.split("::")[1]
                        procedure = procedures.split(",")[0].split("=>")[0]
                        procedure = procedure.strip()
                        functions.append(current_type + "%" + procedure)

    FREE_FUNCTIONS_FILES = ["misc.f90", "chemfiles.f90"]
    for path in FREE_FUNCTIONS_FILES:
        with open(os.path.join(ROOT, "src", path)) as fd:
            for line in fd:
                if "function" in line or "subroutine" in line:
                    if "end" in line or "!" in line:
                        continue
                    name = line.split()[1].split("(")[0]
                    if "internal" in name or name == "chfl_warning_callback":
                        continue
                    functions.append(name)

    return functions
