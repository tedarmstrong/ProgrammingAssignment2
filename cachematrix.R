## Functions that set up a matrix to allow caching of inverse
## Allows quicker operations if using and not constantly revising matrix

## Takes a matrix and gives functions to access that matrix and
## store its inverse once that inverse has been calculated

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calls the solve() function on makeCacheMatrix object to find and 
## cache its inverse, unless that inverse has already been cached,
## in which case it simply returns that cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
