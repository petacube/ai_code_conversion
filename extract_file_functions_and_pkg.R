#!/usr/bin/env Rscript

# Function to parse an R file into AST and save it to a text file
extract_file_dependent_functions <- function(pkg_name, file_name,symbol_output_file,pkg_output_file) {
  #print(file_name)
  df = getParseData(parse(file_name,keep.source=TRUE))
  #print(df)
  symbol_calls = df[df$token == "SYMBOL_FUNCTION_CALL", ]
  pkg_calls = df[df$token == "SYMBOL_PACKAGE", ]
  #print(pkg_calls)
  write.csv(symbol_calls,symbol_output_file,row.names=FALSE)
  write.csv(pkg_calls,pkg_output_file,row.names=FALSE)
}

# Main script to handle command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
  cat("Usage: ./extract_package_functions.R <pkg_name> <file_name> <symbol_output_file> <pkg_output_file> \n")
  quit(status = 1)
}

pkg_name = args[1]
file_name = args[2]
symbol_output_file = args[3]
pkg_output_file = args[4]


# Parse and save the AST
extract_file_dependent_functions(pkg_name,file_name,symbol_output_file,pkg_output_file)
