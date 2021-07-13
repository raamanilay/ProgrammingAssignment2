# get an appropriate variable for a matrix
# the variable x constitutes for the name of the given matrix
makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  put <- function(y) {
    x <<- y
  }
  get <- function() (x)
  putInv <- function(Inverse)(Inv <<- Inverse)
  obtainInv <- function() (Inv)
  list(put = put,  obtain =  obtain, putInv = putInv, obtainInv = obtainInv)
}
 # remaining the given value of the inverse
cacheSolve <- function(x, ...) {
  Inv <- x$obtainInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  mat <- x$get()
  Inv <- solve(mat, ...)
  x$putInverse(Inv)
  Inv
 }
# come back into the inverse of the given variable x matrix
