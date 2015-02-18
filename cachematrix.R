## makeCacheMatrix return a list of functions which implement following API:
##      - setMatrix(Y) --- set a new matrix and 
##                        assign inverseX (a variable for the storing inverse matrix) to NULL
##      - getMatrix() --- return the stored matrix X
##      - setInverseMatrix(inverseMatrix) --- set a new value inverseMatrix for inverseX 
##      - getInverseMatrix() --- return the stored inverse matrix inverseX

makeCacheMatrix <- function(X = matrix()) {
    inverseX <- NULL
    
    setMatrix <- function(Y) {
        X <<- Y
        inverseX <<- NULL
    }
    
    getMatrix <- function() X
    
    setInverseMatrix <- function(inverseMatrix) {
        inverseX <<- inverseMatrix
    }
    
    getInverseMatrix <- function() inverseX
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## cacheSolve return the inverse matrix. 
## If the inverse matrix was computed then it take from a cache.

cacheSolve <- function(X, ...) {
    inverseX <- X$getInverseMatrix()
    
    if (!is.null(inverseX)) {
        return(inverseX)
    }
    
    inverseX <- solve(X$getMatrix(), ...)
    X$setInverseMatrix(inverseX)
    
    inverseX
}
