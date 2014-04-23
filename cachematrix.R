## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## This function creates a special "matrix", which is a list containing a function to
  
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of the inverse matrix
  ## 4. get the value of the inverse matrix
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix function. 
cacheSolve <- function(x, ...) {
  
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then this function retrieves the inverse from the cache.
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


