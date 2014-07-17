## Caching of the Inverse of a Matrix
#
# Matrix inversion being computionally heavy, one could benefit from caching 
# the inverse matrix once computed.
# Later call for a same Matrix Inversion would directly refer to its previous 
# computation result.
#
# Our solution is based on R function `solve` to compute the inverse matrix 
# and uses R Lexical Scoping:
# (Details on scoping http://cran.r-project.org/doc/manuals/R-intro.html#Scope )
#
#
# The solution is divided in a pair function `MakeCacheMatrix` and `cacheSolve`
#
# Notice: Our solution, assume that the matrix supplied is always invertible.

## Example:
#
# source("cacheMatrix.R")
# Generate a random matrix "M" of size 10x10
# M <- matrix(rnorm(100,3,40),10) # Unlikely singular...
# specialM <- makeCacheMatrix(M) # Create a Special "matrix" object
# Minverse <- cacheSolve(specialM) # Compute the inverse of M
## Calling the above line twice would look for the cached version of Minverse
# table(sum(diag(specialM$get() %*% specialM$getinversematrix()))) # Quickly checking inverse

##`MakeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# It is a list containing a function to 
# 1.  set the value of the matrix "M": 
#   makeCacheMatrix$set(M)
# 2.  get the value of the matrix "M": 
#   makeCacheMatrix$get(M)
# 3.  set the value of the inverse matrix of "M": 
#   makeCacheMatrix$setinversematrix(M)
# 4.  get the value of the inverse matrix of "M": 
#   makeCacheMatrix$getinversematrix(M)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) i <<- inversematrix
  getinversematrix <- function() i
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

## `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinversematrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrixdata <- x$get()
  i <- solve(matrixdata, ...)
  x$setinversematrix(i)
  i
}