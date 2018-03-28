## Custom functions to cache matrix inverses

## makeCacheMatrix creates a special matrix with 
## functions to cache its inverse.  The input
## matrix is assumed to be a square invertble matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve evaluates a provided matrix to determine if it
## has a cached inverse. If so, the cached value is returned, 
## if not, the inverse is calculated with solve, cached and returned
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("returning cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
