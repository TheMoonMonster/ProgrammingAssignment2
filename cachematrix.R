## Put comments here that give an overall description of what your
## functions do

## Creates a makeCacheMatrix object, capable of setting/storing a matrix,
## retrieving that matrix, calculating and storing the inverse, and retrieving the inverse if already cached

makeCacheMatrix <- function(x = matrix()) {
      ##sets mean to null
      s <- NULL
      
      ##stores matrix data
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      ## returns matrix data
      get <- function() x
      
      ##sets inverse
      setinverse <- function(solve) s <<- solve
      
      ##retrieves inverse
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)     
}


## returns matrix that is the inverse of x - however, if inverse is already stored
## in x, return from cache instead of recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getinverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setinverse(s)
      s
}
