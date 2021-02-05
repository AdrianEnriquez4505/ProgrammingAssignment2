## Put comments here that give an overall description of what your
## functions do

# The next functions help to compute the inverse of a matrix just once
# and cache its inverse to use in the future

## Write a short comment describing this function

# The first function, makeVector creates a special “matrix”

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
      
      x<<-y
      inverse<<-NULL
    }
    
    get<-function() x
    setinverse <- function(inv) inverse<<-inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix"
# created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("Getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
