# get a variable for a given matrix
# x constitute for the name of the given matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  put <- function(y) {
    x <<- y
  }
  obtain <- function() (x)
  PutInv <- function(inverse)(inv <<- inverse)
  ObtainInv <- function() (inv)
  list(put = put, obtain= obtain, setInverse = putInverse, obtainInverse = obtainInverse)
}
#remaining the value of the given inverse
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
 }
# go back to the inverse of the x matrix
