## We want to cache the inverse of a matrix X as It is a time consuming task. CacheSolve is a function that actually calculates the
## inverse If It doesn't exists yet. This function takes makeCacheMatrix as an argument, used as an empty box to basically store
## and retrieve the inverse matrix. 

##makeCacheMatrix() is basically an empty object shaped to store an maniputale a matrix X and its inverse. It contains 4 functions:
##set, get, setinv and getinv. Get retrieve the original matrix X. Setinv stores the previously calculated inverse matrix in 
##makeCacheMatrix environtment. Getinv retrieves that inverse. At last the main function returns a list with the four child functions

makeCacheMatrix <- function(x) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve recieves as an argument the matrix X and the list of the four functions previously mentioned. It checks whether the
## inverse of X has been calculated using getinv(), If not, It calculates It by using the solve function and returns It.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")  
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}