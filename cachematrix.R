### This is a function that enables to cache the inverse of a matrix rather than 
## compute it repeatedly

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse with 
## the following steps: stores the cached value, initializes to NULL, creates the matrix in the
## working environment, gets the value of the matrix, inverts the matrix and stores in cache,
## gets the inverted matrix from cache and returns the created functions to the working environment

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## This function calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache, it it created in the working environment 
## and it's inverted value is stored in cache

cacheSolve <- function(x, ...) {
  
  cache <- x$getInverse()
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  matrix <- x$get()
  cache <- solve(matrix, ...)
  return (cache)
}

## Example showing how the function works:

a<-matrix(1:4,2,2)
a
b<-makeCacheMatrix(a)
c<-cacheSolve(b)
c
