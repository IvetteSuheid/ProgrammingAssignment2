## DATA SCIENCE SPECIAIZATION
## R Programming: Week 3 Programming Assignment 2: Quiz

## This function creates a special "vector", which is a list containing a function that:
#1)Set the value of the matrix. 
#2)Get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function calculates the inverse of the special "vector" created in makeCacheMatrix.
#It first checks if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of x 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}
