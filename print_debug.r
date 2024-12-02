print_debug <- function(var_name) {
    # Retrieve the line number
    line <- sys.call(-1)[[2]]
    cat(sprintf("[Line %s] %s: Type = %s\n", 
                line, var_name, typeof(var_value)))
  }
