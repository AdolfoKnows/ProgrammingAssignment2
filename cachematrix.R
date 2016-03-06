#Your assignment is to write a pair of functions that cache the inverse of a matrix.
#The makeCacheMatrix prepares the matrix x for the evalaution 
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
#The cacheinvmat solves for the inverse a square matrix. if the matrix has already been solved, it retrieves the previous value. 
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
