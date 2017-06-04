## Compute the inverse of a invertible matrix, caching results

## Create a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setinv <- function(inverse) { inv <<- inverse }
    getinv <- function() { inv }
    invisible(list(set = set, get = get, setinv = setinv, getinv = getinv))
    
}


## Computes the inverse. If it's been calculated, pull the cached value from above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Note: getting cached value...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
