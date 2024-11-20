import sys
import os
import ollama
import glob
from os.path import join, basename, dirname, exists
from parse_pkg_list import parse_description
import time
import openai

from openai import OpenAI
client = OpenAI()

model_name = "gpt-4o-mini"

override = False

if not os.environ.get("OPENAI_API_KEY"):
    raise Exception("OPENAI_API_KEY not defined")

input_folder = sys.argv[1]

input_folder_name = dirname(input_folder)

output_folder_name = basename(input_folder) + "_py_openai"
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
print(import_prefix,flush=True)

for r_code in r_code_files:
    with open(r_code, 'r') as file:
        output_file_name = basename(r_code).split(".")[0] + ".py"
        full_output_file_name = join(output_folder,output_folder_name,output_file_name)
        if not exists(full_output_file_name):
            code = file.read()
            start_time = time.perf_counter()
            input_file_size_kb = os.stat(r_code).st_size/1024.0
            completion = client.chat.completions.create(
                model=model_name,
                messages=[
                {"role": "system", 
         "content": "You are a professional world-class software engineer"},
            {
            "role": "user",
            "content": f"""convert below R code to python. only return python code. all comments should be python-compatible
            keep function names in python same as in R
            do not  emit any markdown as ```python
            {import_prefix} 
            {code}
            """
            }
            ]
            )
            python_converted_code = completion.choices[0].message.content
            elapsed_time = time.perf_counter() - start_time
            print(f"\nExecution Time: {elapsed_time:.4f} seconds")
            output_file_name = basename(r_code).split(".")[0] + ".py"
            with open(full_output_file_name,"w") as output_file:
                output_file.write(python_converted_code)
            output_file_size_kb = os.stat(full_output_file_name).st_size/1024.0
            print(f"validation check: input_file_size (kb) {input_file_size_kb}, output file size (kb) {output_file_size_kb}")
            if output_file_size_kb/input_file_size_kb <0.5:
                    print("looks like significant clipping occured")
        else:
            print(f"skipped processing for {r_code}")

            

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
            completion = client.chat.completions.create(
                model=model_name,
                messages=[
            {"role": "system", 
         "content": "You are a professional world-class software engineer"},
        {
            "role": "user",
            "content": f"""convert below R code to python. only return python code. 
            all comments should be python-compatible.
            keep function names in python same as in R
            do not emit any markdown as ```python
            {import_prefix} 
            {code}
            """
        }
    ]
            )
            python_converted_code = completion.choices[0].message.content

            elapsed_time = time.perf_counter() - start_time
            print(f"\nExecution Time: {elapsed_time:.4f} seconds")
            with open(full_output_file_name,"w") as output_file:
                output_file.write(python_converted_code)
            output_file_size_kb = os.stat(full_output_file_name).st_size/1024.0
            print(f"validation check: input_file_size (kb) {input_file_size_kb}, output file size (kb) {output_file_size_kb}")
            if output_file_size_kb/input_file_size_kb <0.5:
                    print("looks like significant clipping occured")
    else:
        print(f"skipping processing for {r_unit_test}")
        
                     

    
