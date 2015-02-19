## Functions for creating a special "matrix" that can cache it's inverse

## A function that creates a special matrix that can cache it's inverse
## The special matrix has set and get functions, and setinverse and getinverse functions
## which refer to the value of the matrix in the function's environment instance
##
## Arguments x: a matrix. If missing an empty special matrix is created
## Value: The special "matrix"
makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL 
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ix <<- inverse
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache
##
## Arguments:
##   x: a matrix from makeCacheMatrix
##   ...: Extra argument to the function solve
##
## Value: The inverse of x
cacheSolve <- function(x, ...) {
        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}
