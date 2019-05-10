## In this function both the matrix and its inverse are stored into "makeCacheMatrix".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  # By creating a new function, a new environment is used, where x will be stored.
    x <<- y
    inv <<- NULL
  }
  get <- function() x   # obtain x from the "x<<- y"
  setInverse <- function() inv <<- solve(x) # calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## In this function it is checked whether the inverse of the matrix is already available in the variable "inv"
## If this is the case, then it returns inv in the line "return(inv)" and then it stops.
## However, if the variable "inv" is empty, then it will calculate the inverse in this function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
