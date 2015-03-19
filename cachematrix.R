## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setIM <- function(IM) m <<- IM
  getIM <- function() m
  list(set = set, get = get,
       setIM = setIM,
       getIM = getIM)
}


## This function computes the inverse of the special "matrix" or retrieve from the cache

cacheSolve <- function(x, ...) {
  m <- x$getIM()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setIM(m)
  m  
}
