### As far as I could understand, this exercise required minimal changes to the original code provided. I simply
### adapted it to work with matrices (instead of vectors) and return inverses (instead of means)..

###  The following function creates a list of functions for setting ("set") and getting ("get") the value of 
###  a matrix in a cached environment, and also setting and getting the value of the calculated inverse 
###  ("setinverse" and "getinverse").

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


### The following function retrieves the values of the matrix or of its inverse in the cached environment created
### through the makeCacheMatrix () function. It first checks if there is a calculated inverse. In case there is,
### it returns the cached value. In case there isn't 

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #### Retrieves the cached inverse (if it exists)  
  if(!is.null(m)) { #### Checks if the cached inverse is null. If it is not, return the cached value.
    message("getting cached data")
    return(m)
  }
  data <- x$get() ### In case the cached inverse is null, get the original matrix
  m <- solve(data, ...) ### Find the inverse of the original matrix
  x$setinverse(m) ### Set the inverse as the calculated value.
  return(m)
}
}
