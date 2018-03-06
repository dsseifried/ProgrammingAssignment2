## These two functions show how to take a matrix and cache the inverse of a 
## matrix so the same calculations don't have to take place multiple times if
## theinputs are the same

## Shell Function to build other functions to store a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {

# Initialize matrix(cache) to Null
      i <- NULL
      
# Create the matrix and set cache to Null
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
## Get Matrix
      get <- function() x
## Store Inverse of Matrix in Cache
      setinverse <- function(inverse) i <<- inverse
## Get Cache Value
      getinverse <- function() i
## Name function to be able to call from environment
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the cache matrix if it is not 
## already in cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
## If cache it NOT empty, return cache value and exit function      
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
## If cache is empty, get new matrix value, set Inverse, and return inverse
## matrix value

      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      return(i)
}

# Testing Results

# > source('P:/RScripts/ProgrammingAssignment2/cachematrix.R')
# > a<-matrix(1:4,2,2)
# > b<-makeCacheMatrix(a)
# > a
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > b$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# Data is not cached
# > cacheSolve(b)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(b)
# Data is cached
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
