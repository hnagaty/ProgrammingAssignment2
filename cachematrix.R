## Put comments here that give an overall description of what your
## functions do

## A constructor function, it creates a list of functions to handle a cacheable matrix

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



## Get the inverse of a matrix. From cache, if not found in case then it is computed

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

