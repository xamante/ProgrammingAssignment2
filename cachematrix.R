## Functions to create a matrix, look for its inverse and if it does
## not exist then compute it.

## The functions of the object set the value of a matrix and return its value
## it also resets the value of the inverse to NULL 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checks if the inverse of a matrix has already been calculted
## and returns it, if not it computes the inverse.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  data<-x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
