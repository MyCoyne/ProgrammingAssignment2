## My Coyne
## The purpose is to calculate an inverse of an input matrix using cache.
## The file contains two functions:
## makeCacheMatrix() is an object with set, get, setInsverse, getInverse functions
## cacheSolve() function calculates an inverse matrix if it is not already calulated
## 
## Note: input matrix is assumed to be square matrix
##

## The object contains a matrix and access functions to 
##     retrieve and set the matrix and retrieve and set 
##     the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv <<- inv
    getinverse <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## Calculate inverse of the input x if it is not in the cache
##    otherwsie return the inverse matrix that is already 
##              calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
