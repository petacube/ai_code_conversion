import os
import os
import glob
from os.path import join
import sys


target_folder=sys.argv[1]

files = glob.glob(join(target_folder,"*.py"))
for r_code in files:
    print(f"processing {r_code}")
    
    with open(r_code,"r") as r_code_file:
        lines = r_code_file.readlines()
        if len(lines) >0:
            lines = filter(lambda x: "'''" not in x,lines)
            stripped_lines = "".join(lines)
            with open(r_code,"w") as r_code_file_updated:
                r_code_file_updated.write(stripped_lines)

        
