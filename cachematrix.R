## makeCacheMatrix and cacheSolve are utility functions that
## help in caching the result of matrix inversion.
## Usage:
##  # create a cached matrix
##  m <- matrix(1:4, nrow=2, ncol=2)
##  # get inverse
##  cacheSolve(m)

## Create a special matrix that can be used by caching function. It 
## returns a list of 
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  
  # function to set the matrix
  set <- function(y){
    x <<- y
    xInverse <<- NULL
  }
  
  # function to get the matrix
  get <- function() x
  
  # function to get the inverse
  getInverse <- function() xInverse
  
  # function to set the inverse
  setInverse <- function(inverse){
    xInverse <<- inverse    
  } 
  
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverese of a matrix created using makeCacheMatrix
## This function checks if an inverse is already calculated, if not
## it calculates it.
cacheSolve <- function(x, ...) {
    xInverse <- x$getInverse()
    if (!is.null(xInverse)){
      # use the cached value
      message("getting cached data")
      return (xInverse);
    }
  
    # compute the inverse and cache it
    actualMatrix <- x$get()
    xInverse <- solve(actualMatrix)
    x$setInverse(xInverse)
    # return inverse
    xInverse
}
