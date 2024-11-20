import sys
import os
import ollama
import glob
from os.path import join, basename, dirname, exists
from parse_pkg_list import parse_description
import time
from code_cleanup_utils import  strip_lines_with_triple_backticks
import pandas as pd


input_folder = sys.argv[1]

target_package = basename(input_folder)

dependent_pkgs = parse_description(input_folder)

dependent_pkgs.append(target_package)

import_stmt = map(lambda pkg: f"library({pkg})",dependent_pkgs)

import_prefix = "\n".join(import_stmt)

for pkg_name in dependent_pkgs:
    cmd = f"Rscript extract_package_functions.R {pkg_name}"
    os.system(cmd)
    

pivot_wide_func = pd.read_csv("calls.txt")
base_func = pd.read_csv("r_base_functions.txt")
pkg_funcs = {}
pkg_func_map={}
for pkg_name in dependent_pkgs:
    func_list = pd.read_csv(f"{pkg_name}_file_list.txt")
    pkg_funcs[pkg_name] = func_list
#tidyr_file_list.txt
func_list_to_resolve = pivot_wide_func["text"].unique()
for func in func_list_to_resolve:
    if func in base_func["base_functions"].values:
        pkg_func_map[func] = "base"
    else:
        for pkg_name in dependent_pkgs:
            if func  in pkg_funcs[pkg_name]["function_name"].values:
                pkg_func_map[func] = pkg_name
                break
pivot_wide_func["pkg_name"] = pivot_wide_func["text"].map(lambda func: pkg_func_map[func])
pivot_wide_func.to_csv("enriched_calls.txt",index=False)
