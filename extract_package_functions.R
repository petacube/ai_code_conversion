#!/usr/bin/env Rscript

# Function to check and install a package with CRAN or mamba
install_if_missing <- function(package_name) {
  tryCatch({
    # Attempt to load the package
    if (!requireNamespace(package_name, quietly = TRUE)) {
      message(paste("Trying to install package:", package_name, "via CRAN..."))
      install.packages(package_name, dependencies = TRUE,repos = "https://cloud.r-project.org")
      
      # Check if installation was successful
      if (!requireNamespace(package_name, quietly = TRUE)) {
        stop("CRAN installation failed.")
      } else {
        message(paste("Package", package_name, "successfully installed via CRAN."))
      }
    } else {
      message(paste("Package", package_name, "is already installed."))
    }
  }, error = function(e) {
    # Fallback to mamba if CRAN installation fails
    message(paste("CRAN installation failed for", package_name, 
                  ". Attempting installation with mamba..."))
    mamba_command <- paste("mamba install r-", package_name, " -y -c conda-forge", sep = "")
    system(mamba_command, intern = TRUE)
    
    # Verify if mamba installation succeeded
    if (!requireNamespace(package_name, quietly = TRUE)) {
      message(paste("Mamba installation also failed for package:", package_name))
    } else {
      message(paste("Package", package_name, "successfully installed via mamba."))
    }
  })
}


# Function to parse an R file into AST and save it to a text file
extract_package_functions <- function(pkg_name) {
  install_if_missing(pkg_name)
  eval(parse(text=paste0("library","(", pkg_name,")")))
  df = lsf.str(paste0("package:",pkg_name))
  write.csv(df, file = paste0(pkg_name,"_file_list.txt"),row.names=FALSE)

}

extract_package_functions_v2 <- function(pkg_name) {
  # Load necessary library
if (!requireNamespace("utils", quietly = TRUE)) {
  stop("The 'utils' package is required.")
}

install_if_missing(pkg_name)
#eval(parse(text=paste0("library","(", pkg_name,")")))


output_file <- paste0(pkg_name,"_file_list.txt") # Specify the output file name

# Load the package
if (!requireNamespace(pkg_name, quietly = TRUE)) {
  stop(paste("Package", pkg_name, "is not installed."))
}

# Get all objects in the package namespace
all_objects <- data.frame(ls(getNamespace(pkg_name)))

colnames(all_objects) = "function_name"
write.csv(all_objects,output_file,row.names=FALSE)


# Confirmation message
cat(paste("Exported and non-exported functions have been written to", output_file, "in the requested format.\n"))

}

# Main script to handle command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: ./extract_package_functions.R <pkg_name> \n")
  quit(status = 1)
}

pkg_name <- args[1]

# Parse and save the AST
#extract_package_functions(pkg_name)
extract_package_functions_v2(pkg_name)