## These functions are used to store a matrix and to cache the value of its inverse.


## This function consists of a list of functions to set the value of a matrix, get 
## the value of a matrix, set the value of a matrix inverse, and get the value of 
## a matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m_inv <<- inverse
  getinv <- function() m_inv
  list(set = set, get =get, setinv = setinv, getinv = getinv)
  
}


## This function creates the inverse of the matrix created with the above function. If the 
## inverse has already been created the function returns the cached value. Otherwise, it 
## creates the inverse and sets the result in the cache.

cacheSolve <- function(x, ...) {
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  matrix <- x$get()
  m_inv <- solve(matrix, ...)
  x$setinv(m_inv)
  m_inv
}
