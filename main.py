from typing import Dict, Any

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

bases_dict = dict()
replacement_dict = dict()
start_time = 0


        
def time_limit_check(smgr):
    global start_time
    minutes_limit = 10
    should_stop = time.time() - start_time > (60 * minutes_limit)
    if should_stop:
        print("stopped exploration")
    return should_stop


def analyze_func(proj, fun, cfg):
    print(f"started running {fun.name}")
    call_state = proj.factory.call_state(fun.addr, add_options={
        'CALLLESS': True, 'NO_SYMBOLIC_SYSCALL_RESOLUTION': True
    })
    sm = proj.factory.simulation_manager(call_state)
    sm.use_technique(angr.exploration_techniques.LoopSeer(cfg=cfg, bound=2))
    global start_time
    start_time = time.time()
    sm.run(until=time_limit_check)
    print(f"finished {fun.name}")
    return sm


def get_cfg_funcs(proj, binary, excluded):
    """
    get functions that are suitable for analysis, (funcs that are defined in the binary and not libc funcs...)
    """
    return list(filter(None, [f if f.binary_name == binary and (not f.is_plt) and not f.name.startswith(
        "sub_") and not f.name.startswith("_") and f.name not in excluded else None for f in
                              proj.kb.functions.values()]))


def block_to_ins(block: angr.block.Block):
    result = []
    for ins in block.capstone.insns:
        op_str = ins.op_str
        operands = op_str.strip(" ").split(",")
        operands = [i.strip().replace("[","").replace("]", "") for i in operands if i != ""]
        parsed_ins = [ins.mnemonic] + list(filter(None, operands))
        result.append("|".join(parsed_ins).replace(" ", "|") + "|\t")
    return "|".join(result)



def remove_consecutive_pipes(s1):
    s1 = re.sub("(\|(\s)+\|)", "|", s1)
    return re.sub("(\|)+", "|", s1)


def con_to_str(con, replace_strs=[', ', ' ', '(', ')'], max_depth=8):
    repr = con.shallow_repr(max_depth=max_depth, details=con.MID_REPR).replace('{UNINITIALIZED}', '')
    repr=re.sub("Extract\([0-9]+\, [0-9]+\,","",repr)
    for r_str in replace_strs:
        repr = repr.replace(r_str, '|')

    return remove_consecutive_pipes(repr) + "\t"


def gen_new_name(old_name):
    if re.match(r"mem", old_name):
        return 'mem_%s' % old_name.split('_')[2]
    if re.match(r"fake_ret_value", old_name):
        return 'ret'
    if re.match(r"reg", old_name):
        return re.sub("(_[0-9]+)+", '', old_name)
    if re.match(r"unconstrained_ret", old_name):
        return re.sub("(_[0-9]+)+", '', old_name[len("unconstrained_ret_") : ])
    return old_name


def varify_cons(cons, var_map=None, counters=None, max_depth=8):
    """
    abstract away constants from the constraints
    """
    #counters = {'mem': itertools.count(), 'ret': itertools.count()} if counters is None else counters
    var_map = {} if var_map is None else var_map
    new_cons = []
    var_map['Extract'] = ""

    m = None
    for con in cons:
        if con.concrete:
            continue
        for v in con.leaf_asts():
            if v.op in { 'BVS', 'BoolS', 'FPS' }:
                new_name = gen_new_name(v.args[0])
                if re.match(r"mem", new_name):
                    if m is None :
                        m = int(new_name.split('_')[1])
                    else:
                        m = min(m,int(new_name.split('_')[1]))
                var_map[v.cache_key] = v._rename(new_name)
        new_cons.append(con_to_str(con.replace_dict(var_map), max_depth=max_depth))
    final_cons = []
    if m is not None:
        for con in new_cons :
            split = con.split("|")
            for i,s in enumerate(split):
                if re.match(r"mem", s):
                    new_s = 'mem_%d' % (int(s.split('_')[1]) -m)
                    con = con.replace(s,new_s)
            final_cons.append(con)
    else:
        final_cons = new_cons
    return var_map, final_cons



#remove the Numbers from the function names + tokenize the function name.
def tokenize_function_name(function_name):
    name = "".join([i for i in function_name if not i.isdigit()])
    return "|".join(name.split("_"))



def generate_dataset(train_binaries, dataset_name): #keep

    dataset_dir = f"datasets/{dataset_name}"
    os.makedirs(dataset_dir, exist_ok=True)
    analysed_funcs = get_analysed_funcs(dataset_dir)
    for binary in train_binaries:
        analysed_funcs = analyse_binary(analysed_funcs, binary, dataset_dir)


def analyse_binary(analysed_funcs, binary_name, dataset_dir): #keep
    excluded = {'main', 'usage', 'exit'}.union(analysed_funcs)
    proj = angr.Project(binary_name, auto_load_libs=False)
    cfg = proj.analyses.CFGFast()
    binary_name = os.path.basename(binary_name)
    binary_dir = os.path.join(dataset_dir, f"{binary_name}")
    os.makedirs(binary_dir, exist_ok=True)
    funcs = get_cfg_funcs(proj, binary_name, excluded)
    print(f"{binary_name} have {len(funcs)} funcs")
    for test_func in funcs:
        if test_func.name in analysed_funcs:
            print(f"skipping {test_func.name}")
            continue
        print(f"analyzing {binary_name}/{test_func.name}")
        output = open(os.path.join(binary_dir, f"{test_func.name}"), "w")
        analysed_funcs.add(test_func.name)
        try:
            sm: angr.sim_manager.SimulationManager = analyze_func(proj, test_func, cfg)
            sm_to_output(sm, output, test_func.name)
        except Exception as e:
            logging.error(str(e))
            logging.error(f"got an error while analyzing {test_func.name}")
        output.close()
    return analysed_funcs



def get_analysed_funcs(dataset_path): #keep
    binaries = os.scandir(dataset_path)
    analysed_funcs = set()
    for entry in binaries:
        funcs = glob(f"{entry.path}/*")
        analysed_funcs.update(map(lambda x: x[:-len(".pkl")] if x.endswith(".pkl") else x, map(os.path.basename, funcs)))

    return analysed_funcs

def find_target_constants(line):
    targets_mapper = dict()
    targets_counter = itertools.count()
    
    found_targets = set(re.findall(r"jmp\|0[xX][0-9a-fA-F]+|jnb\|0[xX][0-9a-fA-F]+|jnbe\|0[xX][0-9a-fA-F]+|jnc\|0[xX][0-9a-fA-F]+|jne\|0[xX][0-9a-fA-F]+|jng\|0[xX][0-9a-fA-F]+|jnge\|0[xX][0-9a-fA-F]+|jnl\|0[xX][0-9a-fA-F]+|jnle\|0[xX][0-9a-fA-F]+|jno\|0[xX][0-9a-fA-F]+|jnp\|0[xX][0-9a-fA-F]+|jns\|0[xX][0-9a-fA-F]+|jnz\|0[xX][0-9a-fA-F]+|jo\|0[xX][0-9a-fA-F]+|jp\|0[xX][0-9a-fA-F]+|jpe\|0[xX][0-9a-fA-F]+|jpo\|0[xX][0-9a-fA-F]+|js\|0[xX][0-9a-fA-F]+|jz\|0[xX][0-9a-fA-F]+|ja\|0[xX][0-9a-fA-F]+|jae\|0[xX][0-9a-fA-F]+|jb\|0[xX][0-9a-fA-F]+|jbe\|0[xX][0-9a-fA-F]+|jc\|0[xX][0-9a-fA-F]+|je\|0[xX][0-9a-fA-F]+|jz\|0[xX][0-9a-fA-F]+|jg\|0[xX][0-9a-fA-F]+|jge\|0[xX][0-9a-fA-F]+|jl\|0[xX][0-9a-fA-F]+|jle\|0[xX][0-9a-fA-F]+|jna\|0[xX][0-9a-fA-F]+|jnae\|0[xX][0-9a-fA-F]+|jnb\|0[xX][0-9a-fA-F]+|jnbe\|0[xX][0-9a-fA-F]+|jnc\|0[xX][0-9a-fA-F]+|jne\|0[xX][0-9a-fA-F]+|jng\|0[xX][0-9a-fA-F]+|jnge\|0[xX][0-9a-fA-F]+|jnl\|0[xX][0-9a-fA-F]+|jnle\|0[xX][0-9a-fA-F]+|jno\|0[xX][0-9a-fA-F]+|jnp\|0[xX][0-9a-fA-F]+|jns\|0[xX][0-9a-fA-F]+|jnz\|0[xX][0-9a-fA-F]+|jo\|0[xX][0-9a-fA-F]+|jp\|0[xX][0-9a-fA-F]+|jpe\|0[xX][0-9a-fA-F]+|jpo\|0[xX][0-9a-fA-F]+|js\|0[xX][0-9a-fA-F]+|jz\|0[xX][0-9a-fA-F]+ ", line))
    #for target in found_targets:
    #    target = re.sub("0[xX][0-9a-fA-F]+|\|", "" , target)
    #    line = line.replace(target, "jmp")
    for target in found_targets:
        print("removing targets")
        target = re.sub("[a-z]+\|", "" , target)
        if target not in targets_mapper:
            targets_mapper[target] = f"target_{next(targets_counter)}"
    for target, replacement in sorted(targets_mapper.items(), key=lambda x: len(x[0]), reverse=True):
                line = line.replace(target, replacement)
    return line

def sm_to_output(sm: angr.sim_manager.SimulationManager, output_file, func_name):
    counters = {'mem': itertools.count(), 'ret': itertools.count()}
    var_map = {}
    skipped_lines = 0
    constants_mapper = dict()
    constants_counter = itertools.count()
    pos_constants_mapper = dict()
    neg_constants_mapper = dict()
    
    proj = sm._project
    for exec_paths in sm.stashes.values():
        for exec_path in exec_paths:
            blocks = [proj.factory.block(baddr) for baddr in exec_path.history.bbl_addrs]
            processsed_code = "|".join(list(filter(None, map(block_to_ins, blocks))))
            var_map, relified_consts = varify_cons(exec_path.solver.constraints, var_map=var_map, counters=counters)
            relified_consts = "|".join(relified_consts)
            line = f"{tokenize_function_name(func_name)} DUM,{processsed_code}" 
            line = re.sub("r[0-9]+", "reg", line)
            line = re.sub("xmm[0-9]+", "xmm", line)
            line = find_target_constants(line)
            line = re.sub(r"\|[0-9]+", "const", line)
            found_constants = set(re.findall(r"0[xX][0-9a-fA-F]+", line))
                        
            for constant in found_constants:
                if constant not in constants_mapper:
                    constants_mapper[constant] = f"const"
            
            #pos_constants =  set(re.findall(r"\|\+\|[0-9]+", line))
            #neg_constants =  set(re.findall(r"\|\-\|[0-9]+", line))

            #for pos_constant in pos_constants:
            #    pos_constant = re.sub("\|\+\|", "", pos_constant)
            #    if pos_constant  not in pos_constants_mapper:
            #        pos_constants_mapper[pos_constant] = f"pos_const"
            #for neg_constant in neg_constants:
            #    neg__constant = re.sub("\|\-\|", "", neg_constant)
            #    if neg_constant not in constants_mapper:
            #        neg_constants_mapper[neg_constant] = f"neg_const"

            line += f",DUM\n"
            for constant, replacement in sorted(constants_mapper.items(), key=lambda x: len(x[0]), reverse=True):
                line = line.replace(constant, replacement)          
            line = remove_consecutive_pipes(line)
            if len(line) <= 3000:
                print("********************{0}".format(line))
                output_file.write(line)
            else:
                skipped_lines += 1
    print(f"skipped {skipped_lines} lines")


def num_in_sets(set_counts):
    return set_counts['train'] + set_counts['val'] + set_counts['test']


def update_hist(func_hist, name_parts, set):
    for func in name_parts:
        func_counts = func_hist[func]
        func_counts['free'] -= 1
        func_counts[set] += 1
    return func_hist


def set_decide(func_hist, name_parts, global_counters): #keep
    """
    here we tried to devide the inputs between the train/val/test sets such that there is more shared names between the
    sets
    :param func_hist: counters for each name, how many times it appeared in each set
    :param name_parts: names that consist the function name ( '_' seperated function name)
    :return: set to place this function in
    """
    min_func = name_parts[0]
    min_in_set = num_in_sets(func_hist[min_func])
    for func in name_parts:
        if func not in func_hist:
            continue
        curr_in_set = num_in_sets(func_hist[func])
        if curr_in_set < min_in_set:
            if curr_in_set != min_in_set or func_hist[func]['free'] > func_hist[min_func]['free']:
                continue
            min_func = func
            min_in_set = curr_in_set

    min_counts = func_hist[min_func]
    if min_counts['train'] == 0:
        return update_hist(func_hist, name_parts, 'train'), 'train'
    if min_counts['val'] == 0:
        return update_hist(func_hist, name_parts, 'val'), 'val'
    if min_counts['test'] == 0:
        return update_hist(func_hist, name_parts, 'test'), 'test'

    total_samples = sum(global_counters.values())
    if global_counters['train'] / total_samples < 0.7:
        return update_hist(func_hist, name_parts, 'train'), 'train'
    elif global_counters['val'] / total_samples < 0.2:
        return update_hist(func_hist, name_parts, 'val'), 'val'
    else:
        return update_hist(func_hist, name_parts, 'test'), 'test'


def gen_shared_name(func_hist, funcs):
    shared_funcs = []
    for func in funcs:
        if func in func_hist:
            shared_funcs.append(func)
    return shared_funcs


def generate_output(dataset_path, dataset_name): #keep
    """
    this is the experimentation code at the last experiments, we tried to add to the test/val sets only functions that
    have a name part the appeared at least 3 times in the dataset, later we tried to remove from the label the name parts
    that didn't appear more than 3 times, and wrote a function that divides the training functions in a way that
    promotes sharing names across train/val/test sets
    """
    def func_name_extractor(x):
        x = os.path.basename(x)
        if x.endswith(".pkl"):
            return x[:-len(".pkl")]
        return x

    binaries = list(os.scandir(dataset_path))
    import numpy as np
    np.random.seed(42)
    np.random.shuffle(binaries)
    train_output = open(os.path.join(dataset_path, dataset_name + "_train_output.txt"), "w")
    test_output = open(os.path.join(dataset_path, dataset_name + "_test_output.txt"), "w")
    val_output = open(os.path.join(dataset_path, dataset_name + "_val_output.txt"), "w")
    mapper = dict()
    all_funcs = set()
    for i, entry in enumerate(binaries):
        funcs = list(glob(f"{entry.path}/*"))
        all_funcs.update(funcs)
        for func in funcs:
            func_name = func_name_extractor(func)
            func_name = func_name.split("_")
            for label in func_name:
                if label not in mapper:
                    mapper[label] = []
                mapper[label].append(func)

    well_named_funcs = set()
    popular_names = filter(lambda x: len(x[1]) >= 3, mapper.items())
    
    count_func_names = open(os.path.join(dataset_path, "count_func_names.txt"), "w")
    for name, name_funcs in mapper.items():
        line= name + " " + str(len(name_funcs)) + "\n"
        count_func_names.write(line)
    

    names_hists = {name: {'free': len(name_funcs), 'train': 0, 'val': 0, 'test': 0} for name, name_funcs in popular_names}
    for partial in map(lambda x: x[1], filter(lambda x: len(x[1]) >= 3, mapper.items())):
        well_named_funcs.update(partial)
    well_named_funcs = list(well_named_funcs)

    # generate output
    np.random.shuffle(well_named_funcs)
    print(f"{len(all_funcs)} functions, {len(well_named_funcs)} functions with a name that contains a common word")
    # print("choosing 250 functions for test/validation")

    global_counters = {'train': 0, 'val': 0, 'test': 0}
    for i, func in enumerate(well_named_funcs):
        func_name_parts = func_name_extractor(func).split("_")
        print_name = gen_shared_name(names_hists, func_name_parts)
        names_hists, dest = set_decide(names_hists, print_name, global_counters)
        global_counters[dest] += 1
        print_name = "|".join(print_name)
        if dest == 'train':
            output = train_output
        elif dest == 'test':
            output = test_output
        else:
            output = val_output

        print(f"shared name: {print_name}")
        all_funcs.remove(func)
        if func.endswith(".pkl"):
            with open(func, "rb") as f:
                try:
                    sm = pickle.load(f)
                    sm_to_output(sm, output, print_name)
                except Exception as e:
                    print(e)
                    print(f"{func} failed")
        else:
            print("pickle not found")
            with open(func, "r") as f:
                for line in f:
                    line = line.split(" ")
                    line[0] = print_name
                    line = " ".join(line)
                    output.write(line)
    train_output.close()
    test_output.close()
    val_output.close()


def main():
    parser = argparse.ArgumentParser()
    # we did this in order to parallelize the analysis process
    parser.add_argument("--binary_idx", type=int, required=True)
    parser.add_argument("--dataset", type=str, required=True)
    args = parser.parse_args()
    binaries = os.listdir("coreutils_bins")
    #print("Hi , main after args")
    binaries.sort()
    binaries = [f"coreutils_bins/{binary}" for binary in binaries]
    generate_dataset([binaries[args.binary_idx]], args.dataset)
    #print("successfully exited")
    generate_output("datasets/" + args.dataset, args.dataset)


def trim_long_lines(file_path):
    file = open(file_path, "r")
    output_path = os.path.join(os.path.dirname(file_path), f"trimmed_{os.path.basename(file_path)}")
    output_file = open(output_path, "w")
    max_100_lengths = [0] * 100
    counter = 0
    lengths = []

    for line in file:
        curr_min = min(max_100_lengths)
        lengths.append(len(line))
        if len(line) > 2500:
            counter += 1
        else:
            output_file.write(line)
        if len(line) > curr_min:
            max_100_lengths[max_100_lengths.index(curr_min)] = len(line)
    max_100_lengths.sort(reverse=True)
    print(max_100_lengths)
    print(counter)
    import matplotlib.pyplot as plt
    plt.hist(lengths, bins='auto')
    plt.show()


if __name__ == '__main__':
    main()
