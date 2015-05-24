## Assignment: Code two functions that cache and compute the inverse of a matrix.

## Create a special "matrix" object that is able to cache its inverse

makeCacheMatrix <- function(thematrix = matrix()) {
        invmatrix <- NULL
        set <- function(x) {
                thematrix <<- x;
                invmatrix <<- NULL;
        }
        get <- function() return(thematrix);
        setinv <- function(inv) invmatrix <<- inv;
        getinv <- function() return(invmatrix);
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse matrix has
## already been done, cacheSolve must return the cachsedinvere
## matrix.

cacheSolve <- function(thematrix, ...) {
        invmatrix <- thematrix$getinv()
        if(!is.null(invmatrix)) {
                message("Returning cached inverse matrix...")
                invmatrix
        }
        mtxdata <- thematrix$get()
        invmatrix <- solve(mtxdata, ...)
        thematrix$setinv(invmatrix)
        invmatrix
}
