# Caches the operation of reversing a matrix 
# For example, for a very big matrix, it may take too long to compute the inverse, especially if it has to 
# be computed repeatedly (e.g. in a loop). If the contents of a matrix are not changing, it may make sense 
# to cache the value of the inverse so that when we need it again, it can be looked up in the cache rather 
# than recomputed.

# creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverted (solve)
# get the value of the inverted (solve)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}

## calculates the mean of the special "matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
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
