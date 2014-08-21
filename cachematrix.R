## These functions improve performances by caching the 
## inverse of a given matrix that is used often.

## makeCacheMatrix function provides a function list for 
## creating and retrieving an input matrix and inverse 
## in the cache

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## CacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if (!is.null(inverseMatrix)) {
                message("Getting cached data")
                return(inverseMatrix)
        }
        matrixData <- x$get()
        inverseMatrix <- solve(matrixData)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
