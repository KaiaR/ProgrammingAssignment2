## These functions allow to save processing power for certain memory consuming computations in R
## by allowing to save the result to cache for matrices that value does not change


## The first function creates a vector that is function of setting a value of the matrix, getting a value of the matrix, 
## setting an inverse of a matrix and getting an inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The second function returns the value of inverse of x using previously calculated value saved as cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  m
}
