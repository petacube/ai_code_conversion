import sys
import os
import ollama
import glob
from os.path import join, basename, dirname, exists
from parse_pkg_list import parse_description
import tiktoken

model="gpt-4o"

input_folder = sys.argv[1]

input_folder_name = dirname(input_folder)

def num_tokens_from_string(string: str, model_name : str) -> int:
    """Returns the number of tokens in a text string."""
    encoding = tiktoken.encoding_for_model(model_name) 
    num_tokens = len(encoding.encode(string))
    return num_tokens

total_number_of_tokens=0
input_cost_per_mil = 2.5
output_cost_mil = 10.0
output_cost_mil_batch = 5.0
max_input_limit=128*1024
max_output_limit_gpt4o = 64*1024
max_output_limit_llama32 = 4*1024
num_files_exceeding_input_limit=0
num_files_exceeding_output_limit=0

r_code_files=glob.glob(join(input_folder,"R","*.R"))
r_unit_tests = glob.glob(join(input_folder,"tests","testthat","*.R"))


for r_unit_test in r_unit_tests:
    print(f"procesing {r_unit_test}")
    with open(r_unit_test, 'r') as file:
            code = file.read()
            in_token_cnt = num_tokens_from_string(code,model)
            total_number_of_tokens += in_token_cnt
            if in_token_cnt >max_input_limit:
                num_files_exceeding_input_limit +=1
            if in_token_cnt >max_output_limit_gpt4o:
                num_files_exceeding_output_limit +=1
           
                     
for r_code in r_code_files:
    print(f" processing {r_code}")
    with open(r_code, 'r') as file:
            code = file.read()
            in_token_cnt = num_tokens_from_string(code,model)
            total_number_of_tokens += in_token_cnt
            if in_token_cnt >max_input_limit:
                num_files_exceeding_input_limit +=1
            if in_token_cnt >max_output_limit_gpt4o:
                num_files_exceeding_output_limit +=1

# calculate cost
cost_without_batch = input_cost_per_mil*total_number_of_tokens/1e6 + output_cost_mil*total_number_of_tokens/1e6
cost_with_batch = input_cost_per_mil*total_number_of_tokens/1e6 + output_cost_mil_batch*total_number_of_tokens/1e6

print(f" total number of input tokens {total_number_of_tokens}")
print(f" total number of output tokens {total_number_of_tokens}")
print(f" cost without batch ${cost_without_batch}, cost with batch api ${cost_with_batch}")
print(f" num file exceeding input_limit {num_files_exceeding_input_limit}, num files exceeding output limit {num_files_exceeding_output_limit}")

