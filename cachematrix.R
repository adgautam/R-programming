## Caching the Inverse of a Matrix:
## The functions below create an object that stores a matrix and the
## other functio caches the matrix.

## This function creates an object "matrix" that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the object "matrix" created
## earlier by makeCacheMatrix function before. If the inverse has 
## been calculated already, then the function would retrieve the 
## inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("get cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
