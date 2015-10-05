## This file consists of two functions.
## function makeCacheMatrix and function cacheSolve

## makeCacheMatrix creates creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
## set the value of the Matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
## get the value of the Matrix
  get <- function() x
## set the value of the inverse Matrix  
  setinverse <- function(in2) inv <<- in2
## get the value of the inverse Matrix  
  getinverse <- function() inv
## Return a list of cached data
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed). 
## Then the cachesolve should retrieve the inverse from the cache.
## It checks to see if the inverse has already been created at first.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
## check if the inverse has already been created at first.
## If "Yes", return cached Matrix.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
## If "No", compute the inverse and return that.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
