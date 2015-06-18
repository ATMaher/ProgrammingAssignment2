## The following two functions create a special object that
## stores a numeric matrix and caches its inverse

## makeCacheMatrix creates a special "matrix object"; 
## really, it is a list containing the following
## four functions:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matrix_inverse) inv <<- matrix_inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacherSolve returns the inverse of a matrix x
##
## it first checks whether the inverse has been calculated:
## if so, it takes the value from the cache using "getinv";
## if not, it calculates the inverse and sets it in the cache
## using the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
