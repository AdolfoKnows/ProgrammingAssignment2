Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x){
  
  invmatrix <- NULL
  setmatrix <- function(y){
    x<<-y
    invmatrix <<- NULL
  }
  getmatrix <- function()x
  setinvmat <- function(solve)invmatrix<<- solve
  getinvmat <- function() invmatrix
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinvmat=setinvmat, getinvmat=getinvmat)
  
}

cacheinvmat <- function(x, ...) {
  invmatrix <- x$getinvmat()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$getmatrix()
  invmatrix <- solve(data, ...)
  x$setinvmat(invmatrix)
  invmatrix
}
