# A constructor function. The function defines a list of functions that can be used to create a new matrix,
# get its value, set its inverse or get its inverse 

makeCacheMatrix <- function(z, ...) {
  m <- NULL
  x <- matrix(z, ...)
  set <- function(y, ...) {
    x <<- matrix(y, ...)
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This functions get the inverse of a cacheable matrix. If the inverse matrix does exist, then it gets it from 
# the cache, else the inverse is computed.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
