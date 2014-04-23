## These functions privde a way to create and cache invertible matrix

## makeCacheMatrix creates a cacheable version of an invertible matrix. 
##Once a cacheable version of an invertible matrix is created, user can cache and reuse inverse of the matrix using cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of a matrix first time this function is called on an cacheable version of an invertible matrix.
## on all subsequent calls, cached copy of inverse of matrix is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
