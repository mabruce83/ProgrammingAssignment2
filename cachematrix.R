## Put comments here that give an overall description of what your
## functions do

## Creates a list that contains the following functions:
## Set value of matrix
## Get value of matrix
## Get value of the inverse
## Set value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks to see if the inverse of the matrix has been cached.
## If the inverse is cached, it returns it.
## If the inverse is not cached, it calculates the value and caches it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
