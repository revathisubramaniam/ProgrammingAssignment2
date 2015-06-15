## Assignemt 2: A function that cache the inverse of a matrix

## - set the value of the matrix 
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting the cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m    
}
