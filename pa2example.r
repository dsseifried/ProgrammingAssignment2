makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null()) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
}

# This function creates a special "matrix" object that can cache its inverse.      
# makeCacheMatrix <- function(x = matrix()) {
#             
# }

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Solve Function in R

# cacheSolve <- function(x, ...) {
#       ## Return a matrix that is the inverse of 'x'
# }

