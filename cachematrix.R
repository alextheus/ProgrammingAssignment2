## Produces the inverse of a matrix, and caches it if it's not
##already cached.

## This function converts the original matrix into a special object
## that can be cached

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
          x <<- y
          i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function () i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Mirroring the logic of the example, gets the cached inverse
## if available, if not uses the solve function to compute the
## inverse of a given matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
