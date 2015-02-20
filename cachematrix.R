
## Creates a list that allows users to get and set
## a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Stores the inverse of the matrix in a variable
  invertedMatrix <- NULL
  
  ## Sets the matrix
  setMatrix <- function(matrixInput) {
    x <<- matrixInput
    invertedMatrix <<- NULL
  }
  
  ## Gets the matrix
  getMatrix <- function() x
  
  ## Sets the inverse of the matrix
  setInvertedMatrix <- function(invertedMatrixInput) invertedMatrix <<- invertedMatrixInput
  
  ## Gets the inverse of the matrix
  getInvertedMatrix <- function() invertedMatrix
  
  ## Returns the list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
  
}

## Accepts the list created by the makeCacheMatrix
## function and returns the inverse of the matrix
## stored in the list. Also stores the inverse of the
## matrix back into the list
cacheSolve <- function(x, ...) {
  
  ## Gets the inverse of the matrix
  invertedMatrix <- x$getInvertedMatrix()
  
  ## Checks whether the inverse of the matrix
  ## exists. If it exists, returns it.
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  
  ## Gets the matrix
  matrix <- x$getMatrix()
  
  ## Calculates the inverse of the matrix
  invertedMatrix <- solve(matrix)
  
  ## Stores the inverse of the matrix back into the list
  x$setInvertedMatrix(invertedMatrix)
  
  ## Returns the inverse of the matrix to users
  invertedMatrix
  
}
