## There are 2 main functions here in this R script file
## 1) makeCacheMatrix is a function that will return a special matrix 
##    that has setter and getter functions to set/get the input matrix data and
##    set/get the inverse matrix data. If the input matrix data is changed,
##    the inverse matrix data is set to NULL so that it will be calculated when needed
## 2) cacheSolve function takes the special matrix from the makeCacheMatrix function
##    and uses the getter and setters from that to generate / fetch precalculated
##    inverse matrix for the set input matrix. If the input matrix is the same and 
##    inverse matrix was already calculated, then it will use that as is. If the 
##    input to the makeCacheMatrix changed OR if the inverse was not computed yet,
##    then cacheSolve will compute the inverse matrix using the R function named
##    solve and sets it in the special matrix-s cached value.

## This function will take in a matrix object as its input and returns a list 
## of functions that can be used to manipulate the special matrix using those functions
## The returned set of functions can be used to change the input to the function
## or to get the original input provided to the function, to set the inverse matrix
## computed for the input matrix or to get the inverse matrix pre-computed based on the 
## set input matrix. This special matrix function does not validate the input/inverse matrix.
## If the input matrix is changed using the set function, the pre-computed inverse matrix
## will be removed so that the function never returns stale information for the newly
## set input matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list (set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes as its input a list of special matrix functions returned from
## makeCacheMatrix function and computes the inverse matrix if not already computed.
## If the inverse matrix is already computed once and the input matrix for 
## makeCacheMatrix did not change, then this function will return the inverse matrix
## from the cache of the special matrix. If it is not computed already OR if the 
## input matrix was changed, then this function will compute the inverse matrix with
## the help of solve R function and sets it in the special matrix cache for later use.
## This function will always return the inverse matrix computed for the provided input matrix.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## This is a test function to verify that the above functions are working as per the 
## specifications and that the cached inverse matrix is returned when the input remains
## the same and is calculated when the input is changed to the makeCacheMatrix function.
test <- function() {
  a <- makeCacheMatrix (matrix(c(3,3.5,3.2,3.6),2,2))
  b <- makeCacheMatrix (matrix(c(3,3.5,3.2,3.6),2,2))
  c <- makeCacheMatrix (matrix(c(3,3.5,3,3),2,2))
  d <- makeCacheMatrix (matrix(c(3,3.5,3,3),2,2))

  l <- cacheSolve(a)
  l <- cacheSolve(b)
  l <- cacheSolve(c)
  l <- cacheSolve(d)
  l <- cacheSolve(a)
  l <- cacheSolve(c)
  l <- cacheSolve(d)
  l <- cacheSolve(b)
  l <- cacheSolve(c)
  l <- cacheSolve(d)
  l <- cacheSolve(b)
  l <- cacheSolve(a)
  l <- cacheSolve(d)
  l <- cacheSolve(a)
  l <- cacheSolve(c)
  l <- cacheSolve(b)
}
