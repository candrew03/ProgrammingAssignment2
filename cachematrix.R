## The function makeCacheMatrix() creates an R object that stores a vector and its inverse. 
## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() 
## in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment


## This function creates a set of functions and returns the functions within a list to the parent environment.
## It contains four functions: set(), get(), setinverse(), and getinverse()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
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


##This function attempts to retrieve the inverse of a matrix passed in as the argument if a valid cached inverse exists.
##If the inverse has not been calculated, then we calculate the inverse using the solve function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Pushed to GitHub