## Put comments here that give an overall description of what your
## functions do
## makeCatheMatrix creates a special "vector", which is a list of functions to
## 1. Set the matrix
## 2. get the matrix
## 3. set the inverse matrix of the matrix
## 4. get the inverse matrix of the matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(aMatrix){
    x <<- aMatrix
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(iMatrix) inverseMatrix <<- iMatrix
  
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function check if the inverse matrix cached first.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and set it with the setInverseMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  theMatrix <- x$get()
  inverseMatrix <- solve(theMatrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
