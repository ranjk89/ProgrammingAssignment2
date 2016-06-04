## Caching matrix inverse

## Exposing getters, setters for data and inverse matrix, returns these functions as a list

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
         x <<- y
         inverse <<- NULL
      }
      get <- function() x
      setcachematrix <- function(inversematrix) inverse <<- inversematrix
      getcachematrix <- function() x
      list(set = set, get = get,
           getcachematrix = getcachematrix,
           setcachematrix = setcachematrix)
}


## Calculates, caches and returns matrix inverse or returns a cached version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getcachematrix()
        # Is there a cached version
        if(!is.null(inverse)) {
          # Yes, return it
          message("getting cached data")
          return(inverse)
        }
        # No, calculate, cache and return the result
        data <- x$get()
        inverse <- solve(data)
        x$setcachematrix(inverse)
        inverse
}