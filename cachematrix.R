# get an appropriate variable for a matrix
# the variable x constitute for the name of the given matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() (x)
  setInverse <- function(inverse)(inv <<- inverse)
  getInverse <- function() (inv)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
 # remaining the given value of the inverse
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
# come back into the inverse of the given variable x matrix
