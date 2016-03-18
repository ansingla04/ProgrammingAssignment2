## These two functions are used to save time by caching the inverse of a matrix
## instead of calculating it again and again.

## This function takes a matrix as an input and returns a list of 4 functions.
## These 4 functions get and set the matrix, get and set the value of inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of a new matrix and sets its value in the above function.
## If the same matrix is used again, this function uses inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
