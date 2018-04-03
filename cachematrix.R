## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.`cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.Computing the inverse of a square matrix can be done with the `solve` function in R. For example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.

## The first function, `makeCacheMatrix` creates a special "vector", which is really a list containing a function to: 1.  set the value of the matrix, 2.  get the value of the matrix, 3.  set the value of the inverse, 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function()i
  list(setmatrix=setmatrix,
       getmatrix=getmatrix,
       setinverse=setinverse,
       getinverse=getinverse)
}

## The following function calculates the inverse of the special matrix detailed above. However, it first checks to see if the inverse has already been calculate, if so, it get's the inverse from the cache and skips the computation. Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
