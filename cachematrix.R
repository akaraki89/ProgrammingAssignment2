## Functions aiming at calculating the inverse of a square matrix, and handling
## the cache of calculated inverse matrix in order to reduce cost computation
## (taking benefit from caching process)

## The function makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) inv <<- inversematrix
  getinversematrix <- function() inv
  list(set = set, get = get,
       setinversematrix = setinversematrix, getinversematrix = getinversematrix)

}


## The function cacheSolve calculates the inverse of the special "matrix" 
## created with makeCacheMatrix. It first checks to see if the inverse of the 
## matrix has already been calculated (and the matrix has not changed). 
## If so, it gets the inverse of the matrix from the cache and skips 
## the computation. Otherwise it calculates the inverse of the matrix data
## and sets the value of the inverse matrix in the cache via the
## setinversematrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversematrix()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  dataMatrix <- x$get()
  inv <- solve(dataMatrix, ...)
  x$setinversematrix(inv)
  inv
}
