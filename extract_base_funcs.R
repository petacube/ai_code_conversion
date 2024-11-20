#!/usr/bin/env Rscript

# Function to parse an R file into AST and save it to a text file
extract_base_functions <- function() {
  df=data.frame(ls(baseenv()))
  colnames(df) = "base_functions"
  write.csv(df,"r_base_functions.txt",row.names=FALSE)
}

# Parse and save the AST
extract_base_functions()
