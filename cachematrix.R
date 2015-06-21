## makeCacheMatric - creates special list object with capability to store a matrix and its inverse
## cacheSolve - retrieves the inverse if available in cache, or else computes and sets the cache

## creates a special matrix object and the corresponding get and set methods

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x) != ncol(x)) {
    message("non invertibe matrix")
    return
  }
  
  m <- NULL
  
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


## if inverse available uses get method to fetch or else computes and sets teh value of inverse in object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
