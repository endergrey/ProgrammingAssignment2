## cachematrix.R
## Creates a matrix whose inverse can be cached
## Provides functions to set, retrieve, and calculate 
## values for the input matrix and its inverse

## Generate a matrix whose inverse value can be cached
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {       # Sets the value for the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x        # Retrieves the value for the matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Retrieve the cached value for the matrix inverse,
## if available; otherwise, calculate the inverse
## and cache the result
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {         # Checks for cached data
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)     # Computes the inverse
  x$setinverse(inv)
  inv
}
