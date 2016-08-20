## Put comments here that give an overall description of what your
## functions do

## This function contains all functions to set and get the Matrix 
## and it's Inverse Matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix  <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function function creates the inverse matrix for the first time.
## For each next call it returns inverse matrix from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("getting cached data.")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setinverse(invMatrix)
    invMatrix
}
