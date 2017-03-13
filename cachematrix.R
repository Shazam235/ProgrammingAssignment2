## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function create a "special" matrix, 
##which is really a list containing a function to:

#set the value of the matrix

#get the value of the matrix

#set the value of the inverse matrix

#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" 
##created with the above function. 
##However, it first checks to see if the inverse has already been 
##calculated. If so, it gets the inverse from the cache 
##and skips the computation. 
##Otherwise, it calculates the inverse of the matrix 
##and sets the value of the inverse in the cache 
##via the setmean function.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, diag(nrow(data)))
  x$setinv(inv)
}
