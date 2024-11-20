import sys
import os
import ollama
import glob
from os.path import join, basename, dirname, exists
from parse_pkg_list import parse_description
import time
from code_cleanup_utils import  strip_lines_with_triple_backticks

model_name = "llama32_custm"
overwrite=True

input_folder = sys.argv[1]

input_folder_name = dirname(input_folder)

output_folder_name = basename(input_folder) + "_py"
output_folder = join(input_folder_name,output_folder_name)
output_test_folder = join(output_folder,"tests")


os.makedirs(output_folder,exist_ok=True)
os.makedirs(join(output_folder,output_folder_name), exist_ok=True ) 
os.system("touch " + join(output_folder,output_folder_name, "__init__.py"))
os.makedirs(output_test_folder,exist_ok=True )

r_code_files=glob.glob(join(input_folder,"R","*.R"))
r_unit_tests = glob.glob(join(input_folder,"tests","testthat","*.R"))

dependent_pkgs = parse_description(input_folder)

import_stmt = map(lambda pkg: f"library({pkg})",dependent_pkgs)

import_prefix = "\n".join(import_stmt)

files_processed_limit = 1
files_processed_counter=0                     
for r_code in r_code_files:
    print(f"processing {r_code}")
    with open(r_code, 'r') as file:
        r_code_basename = basename(r_code).split(".")[0]
        output_file_name = r_code_basename + ".py"
        output_file_name_test = "test-" + output_file_name
        full_output_file_name = join(output_folder,output_folder_name,output_file_name)
        if ((not exists(full_output_file_name)) or (overwrite is True)):
            code = file.read()
            start_time = time.perf_counter()
            input_file_size_kb = os.stat(r_code).st_size/1024.0
            prompt= f""" 
                                     convert below R code to python. only return python code in plain text. 
            all comments should be python-compatible.
            keep function names in python same as in R
            Please provide the code  without any Markdown formatting or backticks
            dont use any R packages in converted code only equivalent ones eg pandas, numpy and datetime
            dont instantiate any global objects
            
            
                          R code:
                          # R packages the code below may depend on
                            {import_prefix} 
                            
                          # actual R code to be converted
                            {code}
            """
            print(prompt,flush=True)
            response=ollama.generate(model=model_name, 
                    options ={
                    "num_predict": 128*1024
            },
                                     prompt=prompt
                                     )
            elapsed_time = time.perf_counter() - start_time
            print(f"\nExecution Time: {elapsed_time:.4f} seconds")
            python_converted_code = response['response']
            python_converted_code = strip_lines_with_triple_backticks(python_converted_code)
            print(python_converted_code)
            output_file_name = basename(r_code).split(".")[0] + ".py"
            with open(full_output_file_name,"w") as output_file:
                output_file.write(python_converted_code)
            output_file_size_kb = os.stat(full_output_file_name).st_size/1024.0
            print(f"validation check: input_file_size (kb) {input_file_size_kb}, output file size (kb) {output_file_size_kb}")
            if output_file_size_kb/input_file_size_kb <0.5:
                    print("looks like significant clipping occured")
        else:
            print(f"skipped processing for {r_code}")
    files_processed_counter +=1
    # check if matching test exists
    if exists(join(output_test_folder,output_file_name_test)):
        print(f"matching test found! {join(output_test_folder,output_file_name_test)}")
    if (files_processed_counter >= files_processed_limit):
        sys.exit(0)

        # write output to target folder



for r_unit_test in r_unit_tests:
    print(f"procesing {r_unit_test}")
    output_file_name = basename(r_unit_test).split(".")[0] + ".py"
    full_output_file_name = join(output_test_folder,output_file_name)
    if not exists(full_output_file_name):
        with open(r_unit_test, 'r') as file:
            code = file.read()
            input_file_size_kb = os.stat(r_unit_test).st_size/1024.0
            start_time = time.perf_counter()
            response=ollama.generate(model=model_name, 
                                     options ={
                    "num_predict": 128*1024
            }, 
                                     prompt=f""" Instructions: You are professional world-class software engineer:   
                                     convert below R code to python. only return python code. 
            all comments should be python-compatible.
            keep function names in python same as in R
            do not emit any markdown as ```python
                                     R code:
                            {import_prefix} 
                            {code}
            """
            )
            elapsed_time = time.perf_counter() - start_time
            print(f"\nExecution Time: {elapsed_time:.4f} seconds")
 
            
            python_converted_code = response['response']
            with open(full_output_file_name,"w") as output_file:
                output_file.write(python_converted_code)
            output_file_size_kb = os.stat(full_output_file_name).st_size/1024.0
            print(f"validation check: input_file_size (kb) {input_file_size_kb}, output file size (kb) {output_file_size_kb}")
            if output_file_size_kb/input_file_size_kb <0.5:
                    print("looks like significant clipping occured")
    else:
        print(f"skipping processing for {r_unit_test}")
        
