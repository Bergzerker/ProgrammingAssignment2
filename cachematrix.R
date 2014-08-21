## Put comments here that give an overall description of what your
## functions do

## The function below sets m to NULL and prepares a list based on a 
## matrix when passed through it which will be used later to cache the 
## solve() function in the next section of code.

makeCacheMatrix <- function(x = matrix()) {
  # As noted above m is set to NULL each time this function is called
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # Part of list creating functionality of this function.
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function will check if the matrix being passed to it has all ready been solved or not.
## If the matrix has been solved (or the inverse has been found), then it will return
## a cached value.  If not, it will solve for the inverse.  In either case the inverse
## of the matrix passed through it is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m

}
