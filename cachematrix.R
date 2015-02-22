## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse
## A function contaion 4 methods: set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
## This is a method for inverse matrix 
  setinverse <- function(solve) {m <<- solve}
  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' from cach
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the inverse has not already been calculated, then invert the matrix and cach it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
