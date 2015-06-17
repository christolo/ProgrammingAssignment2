## These functions calculate the inverse of a matrix and then cache the result.

## This function creates a list of functions to get and set the value of a calculated 
## matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix if it hasn't been 
## calculated and cached for a particular matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}