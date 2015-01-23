# The first function, makeCacheMatrix creates a special "matrix", 
# a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  inverse <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get the data of matrix
  get <- function() x
  
  # set inverse of the matrix, caculate outside
  setinverse <- function(t) inverse <<- t
  
  # get the inverse of the matrix
  getinverse <- function() inverse
  
  # return the Cachematrix,a list of 4
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special 
## matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse via getinverse function from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  # gets the inverse via getinverse function from the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # calculates the inverse and sets the value of the inverse
  # get the raw matrix data
  data <- x$get()
  # calculate the inverse of the matrix
  i <- solve(data, ...)
  # save it to the cache
  x$setinverse(i)
  # return the inverse
  i
}
