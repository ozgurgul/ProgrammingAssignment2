## Put comments here that give an overall description of what your
## functions do
## A pair of functions that cache the inverse of a matrix.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
      
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      ## Return a matrix that is the inverse of 'x'
      
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
      
}
