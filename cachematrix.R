## Keyton Porras Programming Assignment 2

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a
## matrix rather than computing it repeatedly (there are
## also alternatives to matrix inversion that we will not discuss here).
## The following functions cache the inverse of a matrix. 

## makeChacheMatrix creates a special "matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  INV<- NULL
  set<- function(y) {
    x<<- y
    INV<<- NULL
    
  }
  get<- function() x
  setinverse<- function(inverse) INV<<- inverse
  getinverse<- function() INV
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special 'matrix' created by
## makeCacheMatrix.  If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV<- x$getinverse()
  if(!is.null(INV)){
    message("getting cached data")
    return(INV)
  }
  MAT<- x$get()
  INV<- solve(MAT,...)
  x$setinverse(INV)
  INV
}

