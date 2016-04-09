## This function creates a matrix that can give its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function()x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set , get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}




## This function calculates the inverse of the matrix from above function. If inverse already calculated, then will use the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  i
  
}