## In this file we have two functions that caches a square matrix and calculates
## it's inverse. But the functions are created in such a way that the square matrix
## is cached and also it's inverse, so that in the case that we have big matrices
## and they don't change over time, we can calculate their inverses only once and
## they can be used (calculated) at any moment without long calculations

## This functions creates a Matrix and the functions to set it, get it and set 
## the inverse and get the inverse
## 
## If the matrix changes, it can only be done through the set function, and the 
## set function is done in such a way that the inverse matrix is nulled so it will
## have to be recalculated again


makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y         ## Sets the new matrix
    invM <<- NULL   ## Sets the inverse to NULL in the corresponding environment
  }
  get <- function() x
  setinv <- function(inverseMatrix) invM <<- inverseMatrix
  getinv <- function() invM
  list (set = set, get = get, 
        setinv = setinv, getinv = getinv)
}


## This functions calculates the inverse of matrix x, but if it is cached 
## it will return the cached inverse directly without calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invM <- x$getinv()
  if (!is.null(invM)) {
    message("getting cached inverse matrix")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data)
  x$setinv(invM)
  invM
}
