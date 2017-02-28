## Write a short comment describing this function
## makeCacheMatrix holds a reference to a matrix 'x' and caches the inverse of 'x' through the variable inverse 
## The value NULL of inverse denotes the need to solve for the inverse 
## The value returned exposes the functions get & set for getting and setting the matrix x. 
## In addition, the getinverse and setinverse functions allow for getting and setting the inverse of matrix x. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newInverse) inverse <<- newInverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve will first check if the cachedMatrix returned from makeCacheMatrix has an inverse calculated for it already
## by checking if the inverse returned from getinverse() is NULL.
## If the value returned from getinverse() is not NULL, it will returned the cached result.
## If the value returned is NULL, it will retrieve the wrapped Matrix, through x$get(), calculate the result using the solve function,
## then save the result through x$setinverse, and return the result.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("inverse has been calcuated already, returning cached result")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
