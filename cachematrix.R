##  Below are two functions that are used to create a 
##  special object that stores a matrix and cache's its inverse.

##  MakeCacheMatrix(x) creates a list containing a function to
##  1 set the value of the vector
##  2 get the value of the vector
##  3 set the value of the mean
##  4 get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## creates the set, get, setinverse and getinverse functions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)

}


## cacheSolve(x) calculates the inverse of the list created with makeCacheMatrix(x)
## First, it checks to see if the inverse has already been calculated. 
## 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## Checks if inverse is already in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## When inverse is not in cache, get data and calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  
  x$setinverse(m)
  m
}
