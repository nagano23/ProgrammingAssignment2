##
## Helper functions for inverting matrices
## cache results for re-use
##

## makeCacheMatrix()
##
## Take invertible matrix as parameter,setup
## list of functions for use in cacheSolve()
##
## Quick 3x3 test matrix:
## tm <- 
##  matrix(sample.int(15, 9*100, TRUE), 3, 3)

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}

## Take result of makeCacheMatrix(), return
## inverse. Use existing "cached" result if
## value exists for "m"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
