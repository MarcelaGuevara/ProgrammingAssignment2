## These functions cache the inverse of a matrix by first creating a "special matrix", which 
## is really a list containing a function to set and get the value of the matrix, and set and
## get the value of it's inverse; and then, computes the inverse of the matrix after
## evaluating whether if it has or hasn't been calculated before. 


## This function creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inver) inverse <<- inver
  getInverse <- function(){inverse}
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated, then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached data about the inverse of the matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}