## This file has makeCacheMatrix and cacheSolve. 
## This implements computing matrix inverse and caching it for later retrieval.

## makeCacheMatrix stores matrix inverse in matrixInverse variable 
## Matrix can be set during function call  or with set.
## This function returns list of functions with matrix x and matrixInverse in environment scope

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  set <- function(matrixSquare) {
    x <<- matrixSquare
    matrixInverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inversedMatrix) matrixInverse <<- inversedMatrix
  getInverse <- function() matrixInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve reads inverse from the cached matrix returned by makeCacheMatrix function.
## If null, computes inverse, stores in the scope of makeCacheMatix, and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}