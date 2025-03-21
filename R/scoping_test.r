# Define a global variable
global_var <- "I am global"

# Define function f()
f <- function() {
  # Define a local variable in f()
  local_var_f <- "I am local to f"
  
  # Define function g() within f()
  g <- function() {
    # Define a local variable in g()
    local_var_g <- "I am local to g"
    
    # Access variables
    print(global_var)    # Accessible
    print(local_var_f)   # Accessible
    print(local_var_g)   # Accessible
  }
  
  # Call g() within f()
  g()
  
  # Try to access g()'s local variable (will cause an error)
  # print(local_var_g)  # Not accessible
}

# Call f()
f()
