## The functions cache the inverse of a matrix.

## makeCacheMatrix creates an object that can cache the inverse of the same object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the object returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) { 
          message("getting cached data")
          return(inv)
        }
        
        data<- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
