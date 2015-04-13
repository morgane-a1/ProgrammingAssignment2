## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of matrix x. 
## If the inverse has already been computed, it gets it from cache
## Otherwise it computes it
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
  
}
