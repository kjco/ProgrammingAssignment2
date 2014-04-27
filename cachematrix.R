## The following two functions calculate and cache 
## the inverse of an input matrix.

## The makeCacheMatrix function creates an object that stores an input matrix 
## and caches the inverse of this matrix. This object consists of a list of
## functions with the following purposes:
##   set(): set the data of the matrix
##   get(): get the data of the matrix
##   setMatrixI(): set the calculated inverse of the input matrix
##   getMatrixI(): retrieve the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setMatrixI <- function(MatrixI) im <<- MatrixI
  getMatrixI <- function() im
  list(set=set, get=get, setMatrixI=setMatrixI, getMatrixI=getMatrixI)
}


## The cacheSolve function calculates the inverse of the input matrix stored 
## within the object returned by makeCacheMatrix. If the cache is empty, the 
## inverse of the input matrix will be calculated and cached. If the inverse
## has already been calculated and the input matrix data has not been altered,
## then cacheSolve will directly return the inverse value stored in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x should be an object created by the MakeCacheMatrix function
  im <- x$getMatrixI()
  if (!is.null(im)){
    message("getting cached data")
    return(im)
  }
  matrix_data <- x$get()
  im <- solve(matrix_data,...)
  x$setMatrixI(im)
  im
}
