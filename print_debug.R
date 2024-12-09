print_debug <- function(var_name) {
  # Deparse the argument to get its name as a string
  var_name_str <- deparse(substitute(var_name))
  
  # Print the variable information
  cat(sprintf("[%s] Type = %s\n", 
              var_name_str, typeof(var_name)))
}

print_debug2 <- function(var_name) {
    # Retrieve the line number
    line <- sys.call(-1)[[2]]
    cat(sprintf("[Line %s] %s: Type = %s\n", 
                line, quote(var_name), typeof(var_name)))
  }

test_debug <- function() {
print_debug2(cars)
}

# Example usage

print_debug(cars)

test_debug()

