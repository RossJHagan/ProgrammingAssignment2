## cachematrix extends a basic matrix with the ability to cache solves.  
## Using the setter and getter methods $set() and $get() will 
## ensure that solves are only recalculated when the data has changed.
##
## Use in combination with cacheSolve(x, ...) instead
## of solve(x, ...) to get the cached effect.

## makeCacheMatrix encapsulates a matrix 'x' with caching functionality
## If the inverse is not cached $getinverse will return NULL
## Changes made through the $set method will reset the cache to NULL
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## cacheSolve receives a CacheMatrix and forwards any other
## parameters to the standard solve(x, ...) function.  
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if ( !is.null(i) ) {
    # debug: message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
