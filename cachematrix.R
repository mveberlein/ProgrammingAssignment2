## These functions cache the inverse of a matrix. 

## The function makeCacheMatrix() creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## Stores a list of functions that retrieve or set the value of a matrix and its inverse.
     inv <- NULL 
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse  
     getinverse <- function() inv  
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
     
     
}

## The function cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix(). 
## The inverse is only computed if the inverse has not been computed before.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of invertible matrix x. 
     inv <- x$getinverse()
     if(!is.null(inv)) {
          return(inv)
     }
     data <- x$get()
     inv <-solve(data, ...) 
     x$setinverse(inv)
     inv
     
}