## The function makeCacheMatrix creates a list containing functions to
# set a value of the matrix
# get a value of the matrix
# set the value of inverse, using solve function
# get the value of inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The function cacheSolve calculates the inverse of the matrix, 
# but first checks if it is already created

cacheSolve <- function(x, ...) {
   
  m <- x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  m
    
}
