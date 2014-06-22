## Function calls a cached matrix to speed computing time

## Writes the calculated matrix to cache
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmatrix <- function(recalcmatrix) m <<- recalcmatrix
   getmatrix <- function() m
   list(set = set, get = get,
      setmatrix = setmatrix,
      getmatrix = getmatrix)
}


## Creates the matrix inversion
cacheSolve <- function(x, ...) {
   m <- x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setmatrix(m)
   m
}
