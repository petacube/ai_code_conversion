import os
import sys
from os.path import basename, join, exists
import pandas as pd
import copy

input_code = sys.argv[1]
output_file = basename(input_code).split(".")[0] + ".txt"
temp_output_r = basename(input_code)

def compute_r_parsing(input_code,output_file):
    os.system(f"Rscript parse_R_code.R {input_code} {output_file}")
    source_code = open(input_code,"r").readlines()
    injected_code = copy.deepcopy(source_code)
    parsed_code = pd.read_csv(output_file)
    return (source_code,injected_code,parsed_code)

def persist_temp_output(temp_output_r,injected_code):
    result_h = open(temp_output_r,"w")
    result_h.write("".join(injected_code))
    result_h.close()

source_code, injected_code, parsed_code = compute_r_parsing(input_code=input_code,output_file=output_file)

# inject for each function definition
func_calls = parsed_code[parsed_code["token"] == "FUNCTION"]
function_params = parsed_code[parsed_code["token"] == "SYMBOL_FORMALS"]

# insert tracer after function calls
num_inserts = 0
params_list= []
for idx, line in func_calls.iterrows():
    func_id = line["parent"]
    param = function_params[function_params["parent"] == func_id]["text"]
    code_to_inject = "\n".join(list(map(lambda var: f"print_debug({var})",param))) + "\n"
    
    # determine the function entry point. 
    line_num_to_inject = function_params[function_params["parent"] == func_id]["line1"].max()  + num_inserts
    injected_code.insert(line_num_to_inject,code_to_inject)
    num_inserts +=1

# inject for each function call - ignore base functions
persist_temp_output(temp_output_r,injected_code)

# compute new pair
source_code, injected_code, parsed_code = compute_r_parsing(input_code=temp_output_r,output_file=output_file)

func_calls = parsed_code[parsed_code["token"] == "SYMBOL_FUNCTION_CALL"]
funcs_to_filter = pd.read_csv("analysis/r_base_functions.txt")
func_calls = func_calls[~func_calls["text"].isin(funcs_to_filter["base_functions"])]
func_calls = func_calls[~func_calls["text"].isin(["print_debug"])]
for idx, func_rec in func_calls.iterrows():
    num_inserts=0
    result_line = parsed_code[(parsed_code["token"] == "SYMBOL") & (parsed_code["line1"] <= func_rec["line1"])]["line1"].max()
    result_var = parsed_code[(parsed_code["line1"] == result_line) & (parsed_code["token"] == "SYMBOL")].iloc[0].text
    # find params of the functions
    open_bracket_line_rec = parsed_code[(parsed_code["text"] == "(") & (parsed_code["line1"] >= func_rec["line1"])].iloc[0]
    open_line_num = open_bracket_line_rec["line1"]
    open_bracket_parent_id  = open_bracket_line_rec["parent"]
    closing_bracket_rec = parsed_code[(parsed_code["text"] == ")") & (parsed_code["parent"] == open_bracket_parent_id)]
    closing_bracket_line_num = closing_bracket_rec["line1"].iloc[0]
    params = parsed_code[(parsed_code["token"] == "SYMBOL") & 
                         (parsed_code["line1"] >= open_line_num) & 
                         (parsed_code["line1"] <= closing_bracket_line_num) &
                         (parsed_code["id"] > func_rec["id"])]["text"]
    code_to_inject = "\n".join(list(map(lambda var: f"print_debug({var})",param))) + "\n"
    result_to_inject = f"print_debug({result_var})" + "\n"
    # calcuate entry points - results needs to be printed after function call
    # params need to be printed before function call
    injected_code.insert(result_line,code_to_inject)
    num_inserts+=1
    injected_code.insert(closing_bracket_line_num + num_inserts,result_to_inject)
    num_inserts+=1
    
    persist_temp_output(temp_output_r,injected_code)
    # compute new pair
    source_code, injected_code, parsed_code = compute_r_parsing(input_code=temp_output_r,output_file=output_file)
    
pass

result_h = open(basename(input_code),"w")
result_h.write("".join(injected_code))
result_h.close()

    
        

pass