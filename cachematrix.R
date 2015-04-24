## The makeCacheMatrix and cacheSolve functions below implement 
## functionality to avoid costly computations when calculating 
## the inverse of a matrix.

## ASSUMPTION: The input Matrix "x" should always be invertible.
## this function does not implement any error handling.

##################################
## Function Name: makeCacheMatrix
##################################
## This function returns a list that contains getters and setters 
## for the matrix and its inverse:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse of the matrix
  ## Setter
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  ## getter
  get <- function() x
  ## setter
  setInverse <- function(inverse) inverseMatrix <<- inverse
  ## getter
  getInverse <- function() inverseMatrix
  
  ## returns the list
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##################################
## Function Name: cacheSolve
##################################
## This function returns the inverse of a matrix by first
## checking if the inverse of the matrix is null. If it is not null
## returns the previously calculated (cached) matrix.
## Otherwise, it calculates the inverse using the "solve" function.Then it 
## caches the inverse of the matrix using the setInverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get inverse
  inverseMatrix <- x$getInverse()
  ## if inverse is not null return the cached inverse matrix
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
  
  ## The inverse of the matrix is null, perform 
  ## calculation of the inverse using 'solve' function.
  data <- x$get()
  inverseMatrix <- solve(data)
  
  ## Cache the inverse of the matrix and return this inverse matrix.
  x$setInverse(inverseMatrix)
  inverseMatrix
}

