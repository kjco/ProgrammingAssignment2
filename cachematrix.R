## Put comments here that give an overall description of what your
## functions do
## The following two functions calculate and cache the inverse of an input matrix.

## Write a short comment describing this function
## The makeCacheMatrix function creates an object that stores an input matrix and
## cache's the inverse of this matrix. This function basically creates a list of
## functions with the following purposes:
##   set(): set the data of the matrix
##   get(): get the data of the matrix
##   setMatrixI(): set the calculated Inverse of the input matrix
##   getMatrixI(): retrieve the Inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixI <- function(cal_MatrixI) m <<- cal_MatrixI
  getMatrixI <- function() m
  list(set=set, get=get, setMatrixI=setMatrixI, getMatrixI=getMatrixI)
}

## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the input matrix stored within 
## the object returned by makeCacheMatrix. If the cache is empty, the inverse of the
## input matrix will be calculated and cached. If the inverse has already been 
## calculated and the input matrix data has not been altered, then cacheSolve will
## directly return the inverse value stored in the cache.

cacheSolve <- function(new_x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- new_x$getMatrixI()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix_data <- new_x$get()
  m <- solve(matrix_data,...)
  new_x$setMatrixI(m)
  m
}
