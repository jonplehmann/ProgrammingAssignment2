## makeCacheMatrix is a function that gets and sets the inverse of a matrix. cacheSolve either retrieves the inv (the inverse of the matrix passed to makeCacheMatrix) from memory, or calculates the inverse if the value is NULL.

## creates and stores to memory the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() inv
  setmatrix <- function(solve) inv <<- solve
  getmean <- function() matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## cacheSolve retrieves the inverse of an input matrix from memory, if stored, or sets and returns the inverse if not in cache

cacheSolve <- function(x, ...) {inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
  }
