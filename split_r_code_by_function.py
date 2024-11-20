# Script to split an R file into separate files for each function

# Updated script to split an R file into separate files for each function

import re
import sys
import os

# Replace 'input.R' with the path to your R file containing multiple functions
input_file = sys.argv[1]

# Read the entire file as a single string
with open(input_file, 'r') as f:
    content = f.read()

# Regular expression to match function definitions
# This regex will match function definitions even if they span multiple lines
function_regex = re.compile(
    r'''
    (^|\n)                       # Start at the beginning of the file or after a newline
    \s*                          # Optional whitespace
    ([\w\.]+)                    # Function name (letters, numbers, underscores, dots)
    \s*(<-|=)\s*                 # Assignment operator '<-' or '='
    \s*function\s*\(             # 'function' keyword and opening parenthesis
    [^\{]*                       # Parameters (anything except '{')
    \{                           # Opening brace of function body
    ''',
    re.DOTALL | re.VERBOSE
)

# Function to find matching closing brace
def find_matching_brace(s, start_index):
    stack = []
    for i in range(start_index, len(s)):
        if s[i] == '{':
            stack.append('{')
        elif s[i] == '}':
            if not stack:
                # Unmatched closing brace
                return None
            stack.pop()
            if not stack:
                # All braces matched
                return i
    return None  # No matching closing brace found

# Find all function definitions in the content
matches = function_regex.finditer(content)

for match in matches:
    function_name = match.group(2)
    start_index = match.start()
    open_brace_index = content.find('{', match.end() - 1)

    # Find the matching closing brace
    end_index = find_matching_brace(content, open_brace_index)
    if end_index is None:
        print(f"Could not find matching closing brace for function {function_name}")
        continue

    function_body = content[start_index:end_index+1].strip()

    # Sanitize function name for file name
    file_name = re.sub(r'[^\w]', '_', function_name) + '.R'

    # Write the function to a new file
    with open(file_name, 'w') as f_out:
        f_out.write(function_body)

    print(f"Function {function_name} has been written to {file_name}")
