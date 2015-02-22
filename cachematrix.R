# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a martix and a cached value of the inverse of the
# matrix. Contains the following functions:
# setMatrix to set the value of a matrix
# getMatrix to get the value of a matrix
# cacheInverse to do cache of inverse of the matrix
# getInverse gets the cached value


makeCacheMatrix <- function(x1 = numeric()) {
  # initially nothing is cached so set it to NULL
  mycache1 <- NULL
  # store a matrix
  setMatrix <- function(v1) {
    x1 <<- v1
    # since the matrix is assigned a new value, flush the cache
    mycache1 <<- NULL
  }
  # returns the stored matrix
  getMatrix <- function() x1
  # cache the given argument
  cacheInverse <- function(solve) {
    mycache1 <<- solve
  }
  # get the cached value
  getInverse <- function() mycache1
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# The following function calculates the inverse of a "special" matrix created with
# makeCacheMatrix
cacheSolve <- function(y1, ...) {
  # get the cached value
  inverse1 <- y1$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data1 <- y1$getMatrix()
  inverse1 <- solve(data1)
  y1$cacheInverse(inverse1)
  # return the inverse
  inverse1
}
