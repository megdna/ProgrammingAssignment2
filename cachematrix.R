## This file contains a pair of functions that cache the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the value of the inverse
        setinverse <- function(solve) i <<- solve
        ## Get the value of the inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then cachesolve will retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the data and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Calculate and set the inverse in the cache if not previously computed
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
