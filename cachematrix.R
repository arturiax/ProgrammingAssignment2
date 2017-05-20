## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation so there issome benefit to caching the inverse of a matrix.
# makeCacheMatrix and cacheSolve are two functions that cache the inverse of a matrix.


## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix" object that can cache its inverse. The special "matrix" is a list which contains
#4 functions: "set" to set the matrix, "get" to get the matrix, "setinverse" to set the inverse matrix 
#and "getinverse" to get the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inversematrix) im <<- inversematrix
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

#cacheSolve calculates the inverse of a singular matrix using the "solve" function, but it first checks to see 
#if the inverse has already been calculated and so then skips computation. If a new inverse matrix is calculated, 
#cacheSolve set the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}

