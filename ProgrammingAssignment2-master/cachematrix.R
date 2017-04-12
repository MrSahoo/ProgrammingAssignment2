## Following functions create an object that stores a matrix
## and caches its inverse

## makeCacheMatrix creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
      set <- function(y) {
      	x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## The function solves a matrix that is inverse of 'x'

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if (!is.null(inverse)) {
      	message("getting cached data")
            return(inverse)
      }
      mat <- x$get()
      inverse <- solve(mat, ...)
      x$setinverse(inverse)
      inverse
}
