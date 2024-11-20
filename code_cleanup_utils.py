def strip_lines_with_triple_backticks(text):
    # Split the text into lines
    lines = text.split('\n')
    
    # Filter out lines that start with triple backticks
    filtered_lines = [line for line in lines if not line.startswith('```')]
    
    # Join the remaining lines back into a single string
    return '\n'.join(filtered_lines)



