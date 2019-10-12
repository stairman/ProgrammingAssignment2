## Using these functions, we can create special matrix, and solve its inverse matrix.

## This function creates special matrix object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invers <- NULL
    set <- function(y) {
      x <<- y
      invers <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) invers <<- solve
    getInverse <- function() invers
    list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This function computes the inverse of special matrix which is created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    invers <- x$getInverse()
    if(!is.null(invers)) {
      message("getting cached data")
      return(invers)
    }
    data <- x$get()
    invers <- solve(data, ...)
    x$setInverse(invers)
    invers
}

