## For most time consuming computations, it’s good practise to cache the results and retrive it later 
## instead of computing them again. Following are the example of creating maxtrix that can cache 
## its inverse (method makeCacheMatrix()) and (method cacheSolve()) computes the inverse of the
## matrix. If the inverse has already been calculated, it will retrieve the inverse from the cache directly
## otherwise calulate the inverse and set it in the cache


## makeCacheMatrix -- This method creates a square invertible matrix object that can cache its inverse. 
## It returns a list which contains 4 functions to set & get matrix object and 
## setmatrix & getmatrix functions to set and get inverse of the matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) inv <<- matrix
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve -- This method takes square invertible matrix as an input and computes the inverse of it.
## If the inverse has already been calculated then this method retrives the inverse   
## from the cache and returns it otherwise it calculates inverse and set it in the cache then returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
}
