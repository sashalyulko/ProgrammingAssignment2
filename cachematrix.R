## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a vector of functions that do the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 	  xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) xinverse <<- solve
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSove first checks if the value of the inverse matrix exists in cache
## and if it does, skips calculations. If there is no previous value of the
## inverse matrix, it calculates the inverse and sets it to cache using the
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinverse(xinverse)
        xinverse
}
