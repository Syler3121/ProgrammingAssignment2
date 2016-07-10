## Put comments here that give an overall description of what your
## functions do
  # makeCacheMatrix creates a list containing a function to
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the value of inverse of the matrix
  # 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  ## Initialize invse
  inv <- NULL
  
  ## Store the value of the input matrix into cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Get the value of the input matrix from cache
  get <- function() x
  ## Store the inverse of the matrix into cache
  setinv <- function(solve) inv <<- solve
  ## Get the inverse of the matrix from cache
  getinv <- function() inv
  ## Store all of those functions list
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


  # The following function returns the inverse of the matrix.First it checks if
  # the inverse has already been computed. If yes, it gets the result and skips the
  # computation. If no, it computes the inverse, sets the value in the cache via
  # the function setinverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check to see if the inverse is already in the cache
  inver <- x$getinv()
  
  ## If the inverse is in the cache, put a message which states we got the inverse from
  ## the cache, and return the inverse matrix
  if(!is.null(inver)) {
    ## Notify the user the inverse was already calculated and is just being taken from cache
    print("Inverse was already calculated and stored in cache. Getting cached inverse.")
    return(inver)
  }
  ## Otherwise get the inverse of the matrix, and notify the user the inverse is being calculated
  ## in this run.
  else {
    ## Notify the user that this is the first run and that the inverse is being calculated
    print("First time inverse is being calculated")
    ## Get the value of the matrix
    data <- x$get()
    ## Solve for the inverse of the matrix
    inver <- solve(data)
    ## Now set the inverse of that matrix in the cache for future use
    x$setinv(inver)
    ## Return that inversed matrix
    inver
  }
}