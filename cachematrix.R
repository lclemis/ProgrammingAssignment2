## Two functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  # xMatrixInversion stores the result of the matrix inversion
  xMatrixInversion <- NULL
  set <- function(y) {
    x <<- y
    xMatrixInversion <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) xMatrixInversion <<- inverseMatrix
  getInverseMatrix <- function() xMatrixInversion
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of a special "matrix" returned by the makeCacheMatrix. 
## If the inverse has already been calculated, then this function should retrieve the inverse from the cache.
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  xMatrixInversion <- x$getInverseMatrix()
       if(!is.null(xMatrixInversion)){
         message("getting cached data")
         return(xMatrixInversion)
       }
  data <- x$get()
  xMatrixInversion <- solve(data)
  x$setInverseMatrix(xMatrixInversion)
  xMatrixInversion
}
