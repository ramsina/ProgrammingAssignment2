## The following gets and sets a matrix as well gets and sets the matrix's inverse.
## It then cache's the inverse matrix

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
       getsolve = getsolve)

}
## The following function will try to get the inverse of a given matrix. 
## But first it will check if the inverse exists. If the matrix has already
## been inverted then it will get it from the cache and stop the rest of the 
## computation

cacheSolve <- function(x, ...) {

    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m}


    
