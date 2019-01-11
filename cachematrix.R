## makeCacheMatrix is a function that creates a special "matrix" object that
## can cache its inverse

## x is defined as a square invertible matrix
## i is the cache for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {

## use <<- to assign a value to an object in an environment that is different
## from the current environment

          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve function calculates the inverse of the special "matrix" created
## with makeCacheMatrix

cacheSolve <- function(x, ...) {
     i <- x$getinverse()

## checking if the inverse is already calculated
          if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     
## else calculate the inverse matrix
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)

## print i (inverse matrix)
     i
}
