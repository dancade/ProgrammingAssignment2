## Functions to calculate the inverse of a matrix and stores it
## to use it on future calculations in case the original matrix is the same

setwd("C:/Users/edancad/Documents/Coursera/Data Science Specialization/Course2/ProgrammingAssignment2")

## makeCacheMatrix creates a list with the following function: set a matrix,
## get a matrix, set inverse of the matrix, get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(matrixToSet){
        x <<- matrixToSet
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## cacheSolve function takes as an argument a list of type makeCacheMatrix
## and calculates the inverse of the matrix if it has not been calculated before,
## otherwise it returns stored value of the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}    