import angr
import os
import pickle
import re
import time
import logging
import json
import argparse
import itertools
from glob import glob

def get_cfg_funcs(proj, binary, excluded):
    """
    get functions that are suitable for analysis, (funcs that are defined in the binary and not libc funcs...)
    """
    return list(filter(None, [f if f.binary_name == binary and (not f.is_plt) and not f.name.startswith(
        "sub_") and not f.name.startswith("_") and f.name not in excluded else None for f in
                              proj.kb.functions.values()]))


def analyze_func(proj, fun, cfg):
    print(f"started running {fun.name}")
    call_state = proj.factory.call_state(fun.addr, add_options={
        'CALLLESS': True, 'NO_SYMBOLIC_SYSCALL_RESOLUTION': True
    })
    # dropped the relativization in the last moment due to time consedirations, and we think that the address_breakfun


  # need to be checked again...
    # call_state.inspect.b('address_concretization', when=angr.BP_AFTER, action=address_breakfun)
    sm = proj.factory.simulation_manager(call_state)
    sm.use_technique(angr.exploration_techniques.LoopSeer(cfg=cfg, bound=2)) 
    sm.run()
    return sm


def run(binary_name):
    proj = angr.Project(binary_name, auto_load_libs=False)
    cfg = proj.analyses.CFGFast()
    funcs = get_cfg_funcs(proj, binary_name, {})
    for test_func in funcs:
        sm: angr.sim_manager.SimulationManager = analyze_func(proj, test_func, cfg)
        for exec_paths in sm.stashes.values():
            for exec_path in exec_paths:
                print("printing func")
                print(test_func)
                print("printing constraint")
                print(exec_path.solver.constraints)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--binary_name", type=str, required=True)
    args = parser.parse_args()
    run(args.binary_name)

