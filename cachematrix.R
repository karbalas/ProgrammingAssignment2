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
  ## sets the initial value for the inverse matrix to NULL
  i <- NULL
  
  ## set function to set the input matrix for the makeCacheMatrix.
  ## This function will also set the dirty bit for the pre-computed
  ## inverse matrix by setting it to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get function to get the input matrix used for the makeCacheMatrix function
  get <- function() x
  
  ## setinverse function to set the computed inverse matrix in to the cache 
  ## of the special matrix.
  setinverse <- function(inverse) i <<- inverse
  
  ## getinverse function to get the computed inverse matrix for the provided
  ## input matrix to the makeCacheMatrix function.
  getinverse <- function() i
  
  ## returns a list of functions to manipulate or use the special matrix
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
  ## fetches the pre-computed inverse matrix from the special matrix, if available.
  m <- x$getinverse()
  
  ## if inverse matrix is already computed and the input has not changed
  if(!is.null(m)) {
    message("getting cached data")
    ## returned the cached inverse matrix object as is
    return(m)
  }
  
  ## If we are here, then the inverse matrix was NOT computed for the 
  ## given input matrix. This could be the first time we are accessing the 
  ## inverse matrix after setting the input matrix for the makeCacheMatrix function.
  
  ## get the input matrix provided to the makeCacheMatrix function
  data <- x$get()
  
  ## using the solve R function, compute the inverse matrix
  m <- solve(data, ...)
  
  ## cache the inverse matrix computed above so that subsequent invocations
  ## to get the inverse matrix can reuse the value unless the input matrix is changed.
  x$setinverse(m)
  
  ## returns the newly computed inverse matrix.
  m
}

## This is a test function to verify that the above functions are working as per the 
## specifications and that the cached inverse matrix is returned when the input remains
## the same and is calculated when the input is changed to the makeCacheMatrix function.
test <- function() {
  ## To test, we need multiple input matrixes.
  
  ## Creating makeCacheMatrix-es a, b, c and d with different input matrix values
  ## Though a and b uses the same input, they are different R objects in scope of fn.
  ## Similarly, c and d are structurally the same matrixes functions, but scoped input
  ## values do not represent the same R objects.
  a <- makeCacheMatrix (matrix(c(3,3.5,3.2,3.6),2,2))
  b <- makeCacheMatrix (matrix(c(3,3.5,3.2,3.6),2,2))
  c <- makeCacheMatrix (matrix(c(3,3.5,3,3),2,2))
  d <- makeCacheMatrix (matrix(c(3,3.5,3,3),2,2))


  ## invoking cacheSolve with a, b, c and d should not use cached values
  ## as they all are invoked on different scoped function / environments
  message("Start testing first time computation of inverse matrixes")
  l <- cacheSolve(a)
  l <- cacheSolve(b)
  l <- cacheSolve(c)
  l <- cacheSolve(d)
  message("End testing first time computation of inverse matrixes")
  
  ## However, invoking a, b, c or d AGAIN with the same scope as before,
  ## should use the cached information and should not re-compute inverse matrixes.
  message("Start testing usage of inverse matrixes from cached values")
  l <- cacheSolve(a)
  l <- cacheSolve(c)
  l <- cacheSolve(d)
  l <- cacheSolve(b)
  
  ## no matter how many times they are invoked.
  l <- cacheSolve(c)
  l <- cacheSolve(d)
  l <- cacheSolve(b)
  l <- cacheSolve(a)
  message("End testing usage of inverse matrixes from cached values")
  
  ## But if the input matrix is changed for that scope, it should re-compute
  ## inverse matrix rather than using the stale cached values.
  ## To test that, we will switch input matrixes for the functions and see

  message("Start testing recomputation of inverse matrixes")
  #gets the input matrix from makeCacheMatrix function referenced by 'b'
  temp <- b$get() 

  ## sets 'b' functions input to 'a' functions input
  b$set(a$get())
  l <- cacheSolve(b)
  
  ## sets 'a' functions input to 'c' functions input
  a$set(c$get())
  l <- cacheSolve(a)
  
  ## sets 'c' functions input to 'd' functions input
  c$set(d$get())
  l <- cacheSolve(c)
  
  ## sets 'd' functions input to 'b' functions input
  d$set(temp)
  l <- cacheSolve(d)
  message("End testing recomputation of inverse matrixes")
}
