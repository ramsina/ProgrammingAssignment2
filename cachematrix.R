## trying to set up a function that will create a special matrix object that can cache
# its inverse

makeCacheMatrix <-function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}
## tries to invert the matrix but checks if the inverted matrix already exists first

cacheSolve <- function(x, ...) {

    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m}

## inverse of matrix
    
