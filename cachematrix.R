## Contains functions to cache computed inverses of special caching matrix objects
## First, create such an object by calling makeCacheMatrix
## Then get or compute the cached inverse by passing the result of makeCacheMatrix to cacheSolve

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
      return(inverse)
    }
    m <- x$get()
    inverse <- solve(m)
    x$setInverse(inverse)
    inverse
}
