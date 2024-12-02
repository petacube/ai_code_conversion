#!/usr/bin/env Rscript

# Function to parse an R file into AST and save it to a text file
parse_R_code <- function(file_name, output_file) {
  df = getParseData(parse(file_name,keep.source = TRUE))
  write.csv(df, output_file, row.names = FALSE)
}

# Main script to handle command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: ./parse_R_code.R <input_file> <output_file> \n")
  quit(status = 1)
}

input_file = args[1]
output_file = args[2]


# Parse and save the AST
parse_R_code(input_file,output_file)



