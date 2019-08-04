## 
## The functions makeCacheMatrix and cacheSolve have been designed as second
## assignment of the "R Programming" course over the Coursera platform. The two 
## functions allow calculating and storing the inverse of a matrix in order to
## reduce the number of iterations executed by the system.

## The function makeCacheMatrix creates an object with two variables:
## - m: the matrix passed as function's parameter
## - i: the variable used to store the inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
  # Inizialize the variables with NULL
  i <- NULL

  # The matrix used as parameter of the function is assigned to the variable m
  set <- function(p) {
    m <<- p
    i <- NULL
  }
  
  # The variable m can be retrieved
  get <- function() {
    return(m)
  }
  
  # Assign the function's input to the variable i, which is used to store the inverse
  setsolve <- function(inv) {
    i <<- inv
  }
  
  # Return the inverse stored in the variable i
  getsolve <- function() {
    return(i)
  }
  
  # List of functions inside the object
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve 

cacheSolve <- function(x, ...) {
  # Retrieve the inverse stored in the function's parameter
  i <- x$getsolve()
  
  # If the value stored is not null, the program shows it and ends
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise the program retrives the matrix, calculates the inverse and stores it
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)

  # Return a matrix that is the inverse of 'x'  
  i
}
