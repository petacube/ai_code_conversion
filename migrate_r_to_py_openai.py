import sys
import os
import ollama
import glob
from os.path import join, basename, dirname, exists
from parse_pkg_list import parse_description
import time
import subprocess
import logging
from code_cleanup_utils import  strip_lines_with_triple_backticks

from openai import OpenAI
client = OpenAI()

logging.basicConfig(level=logging.DEBUG)

#model_name = "gpt-4o-mini"
#model_name = "o1-mini"
model_name="o1-preview-2024-09-12"
#model_name = "gpt-4o"

max_iterations=2
override = False

if not os.environ.get("OPENAI_API_KEY"):
    raise Exception("OPENAI_API_KEY not defined")

input_folder = sys.argv[1]

input_folder_name = dirname(input_folder)

output_folder_name = basename(input_folder) + "_py_openai"
output_folder = join(input_folder_name,output_folder_name)
output_code_folder = join(output_folder, output_folder_name)
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


def execute_command_and_capture_output(cmd):
    """_summary_

    Args:
        cmd (_type_): _description_
        example cmd = ["python", "setup.py", "build"]
    """
    

    

    logging.debug("Executing command: %s", " ".join(cmd))


    # Command to execute
    #command = ["python", "setup.py", "build"]

    try:
    # Run the command and capture output
        result = subprocess.run(
        cmd,
        capture_output=True,  # Capture stdout and stderr
        text=True  ,         # Get output as string instead of bytes,
        shell=False
    )

        # Check return code
        if result.returncode != 0:
            return result.stdout + " " + result.stderr  # Print the captured error output
        elif ("SyntaxError" in result.stderr) \
        or("AssertionError" in result.stderr) or \
        ("FAILED" in result.stderr):
            return result.stderr
        elif ("SyntaxError" in result.stdout) or \
             ("AssertionError" in result.stdout) or \
             ("FAILED" in result.stdout):
            return result.stdout
        else:
            print("Build completed successfully!")
            return None  # Print the captured success output

    except Exception as e:
        print(f"An error occurred: {e}")



def verify_code_quality(file_path,unit_test,base_folder):
    current_dir=os.getcwd()
    os.chdir(base_folder)
    cmd1 = ["python", "-m","py_compile",  file_path]
    cmd2 = ["python", "setup.py", "install"]
    cmd3 = ["pytest",f"{unit_test}"]
    
    result = execute_command_and_capture_output(cmd1)
    
    if result is not None:
        os.chdir(current_dir)
        return result
    
    result = execute_command_and_capture_output(cmd2)
    if result is not None:
        os.chdir(current_dir)
        return result
    
    """ don't execute unit test
    result = execute_command_and_capture_output(cmd3)
    if result is not None:
        os.chdir(current_dir)
        return result
    """
    
    os.chdir(current_dir)
    return None

def convert_code(model_name="",import_prefix="",code="",feedback_msg=None,previous_code=None,mode="code",pkg_name=""):
    if mode == "code":
        if feedback_msg is None:
            custom_prompt = f"""You a software engineer expert in R and Python. 
            convert below R code to python. only return python code. 
            all comments should be python-compatible
            keep function names in python same as in R
            do not  emit any markdown
            
            Hint:
            when constructing pandas Series with custom attributes be aware 
            pd.Series constructor does not have an attrs parameter.     
            Therefore to work around this issue create the Series first without the attrs parameter.
            Then, set the attrs attribute separately.
            
            {import_prefix}
            
            {code}
            """
        else:
            custom_prompt = f"""You a software engineer expert in R and Python. 
            convert below R code to python. only return python code. 
            all comments should be python-compatible
            keep function names in python same as in R
            do not  emit any markdown
            
            Hint:
            when constructing pandas Series with custom attributes be aware 
            pd.Series constructor does not have an attrs parameter.     
            Therefore to work around this issue create the Series first without the attrs parameter.
            Then, set the attrs attribute separately.
            
            
            Feedback from previous iteration: {feedback_msg}
            Code you have generated before {previous_code}
            
            {import_prefix}
            
            {code}
            """
    else:
            # assume mode is test
            custom_prompt = f"""You a software engineer expert in R and Python. 
            convert below R unit test code testthat to python equivalent of pytest. 
            only return python code.
            assume you migrating code for python package {pkg_name}
            and all functions you need are defined there. 
            use pandas data types that can support None value without conversion to np.nan eg Int64, string etc.
            always specify column data types explicitly when possible in pandas dataframes using Series and dtypes
            
            Hint:
            when constructing pandas Series with custom attributes be aware 
            pd.Series constructor does not have an attrs parameter.     
            Create the Series first without the attrs parameter.
            Then, set the attrs attribute separately.
            
            {import_prefix}
            
            {code}
            """
    logging.debug(f"sending prompt to llm {custom_prompt}")
    
    completion = client.chat.completions.create(
                model=model_name,
                messages=[
            {
            "role": "user",
            "content": custom_prompt
            }
            ]
            )
    python_converted_code = completion.choices[0].message.content
    python_converted_code = strip_lines_with_triple_backticks(python_converted_code)
    return python_converted_code

def find_matching_unit_test(fname):
    bname = basename(fname)
    bname_no_ext = bname.split(".")[0]
    test_name = f"test-{bname}"
    if exists(join(input_folder,"tests","testthat",test_name)):
        return join(input_folder,"tests","testthat",test_name)
    else:
        return None

def cleanup_file_name(fname):
    fname_updated = fname.replace("-","_")
    return fname_updated

def update_init_file(pkg_name,module_name, init_path):
    #"from tidyr_py_openai.drop_na import drop_na"
    init_lines=open(join(init_path,"__init__.py"),"r").read()
    if f"from {pkg_name}.{module_name}" not in init_lines:
        init_file_handle = open(join(init_path,"__init__.py"),"a")
        init_file_handle.write(f"from {pkg_name}.{module_name} import *\n")
        init_file_handle.close()
    
    
def convert_one_file(file_name="", output_folder="",import_prefix="",feedback_msg=None, old_code=None,mode="code",pkg_name=""):
    print(f"procesing {file_name}")
    output_file_name = basename(file_name).split(".")[0] + ".py"
    output_file_name = cleanup_file_name(output_file_name)
    full_output_file_name = join(output_folder,output_file_name)
    if (not exists(full_output_file_name)) or (override is True):
        with open(file_name, 'r') as file:
            code = file.read()
            input_file_size_kb = os.stat(file_name).st_size/1024.0
            start_time = time.perf_counter()
            python_converted_code = convert_code(model_name=model_name,
                                                 import_prefix=import_prefix,
                                                 code=code,
                                                 feedback_msg = feedback_msg, 
                                                 old_code = old_code,
                                                 mode=mode,
                                                 pkg_name=pkg_name)
            elapsed_time = time.perf_counter() - start_time
            print(f"\nExecution Time: {elapsed_time:.4f} seconds")
            with open(full_output_file_name,"w") as output_file:
                output_file.write(python_converted_code)
            output_file_size_kb = os.stat(full_output_file_name).st_size/1024.0
            print(f"validation check: input_file_size (kb) {input_file_size_kb}, output file size (kb) {output_file_size_kb}")
            if output_file_size_kb/input_file_size_kb <0.5:
                    print("looks like significant clipping occured")
    else:
        print(f"skipping processing for {file_name}")
    return full_output_file_name

for r_code in r_code_files:
    full_output_file_name = convert_one_file(file_name=r_code,
                                             output_folder=output_code_folder,
                                             import_prefix=import_prefix,
                                             feedback_msg=None,
                                             old_code="",
                                             mode="code",
                                             pkg_name=output_folder_name)
    module_name = basename(full_output_file_name).split(".")[0]
    update_init_file(pkg_name=output_folder_name,module_name = module_name, init_path=output_code_folder)
    matching_unit_test = find_matching_unit_test(r_code)
    if matching_unit_test is not None:
            convert_one_file(file_name=matching_unit_test,
                             output_folder=output_test_folder,
                             import_prefix="",
                             feedback_msg=None,
                             old_code=""
                             mode="test",
                             pkg_name=output_folder_name)
            feedback=verify_code_quality(full_output_file_name,matching_unit_test,output_folder)
            if feedback is not None:
                # something, syntax fail or unit test fail
                current_iteration=0
                while (current_iteration < max_iterations):
                    print(f"Error detected, sedning feedback to llm {feedback}")
                    full_output_file_name = convert_one_file(file_name=r_code,
                                                             output_folder = output_code_folder,
                                                             import_prefix=import_prefix,
                                                             feedback_msg= feedback,
                                                             old_code=open(full_output_file_name,"r").read(),
                                                             mode="code",
                                                             pkg_name=output_folder_name
                                                             )
                    feedback=verify_code_quality(full_output_file_name,matching_unit_test,output_folder)
                    if feedback is None:
                        break
                    current_iteration +=1
        
    else:
            print("Unit test is missing! cannot verify")
            
    
