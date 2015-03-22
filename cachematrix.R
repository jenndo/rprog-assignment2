## This pair of functions cache the inverse of a matrix to avoid repeated calculations
## if the matrix has not changed

## makeCacheMatrix is a function creating a special matrix object that can cache 
## the inverse of itself. This function utilize the <<- operator, which assigns 
## a value to an object in an environment that is different from the currrent environment

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse of the special matrix returned by makeCacheMatrix.
## If the matrix has not changed and the inverse has been already calculated, cacheSolve will simply
## pull the matrix inverse from cache. If the matrix is changed, cacheSolve will calculate its inverse 
## and store it back in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data ...")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}
