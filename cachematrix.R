## It is set of functions that allow to cache matrix
## and inversion of the matrix

## The function provides ability to put and get 
## matrix from cache as well as its inverse version

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## function checks the cache for inversed version of the matrix x
## if it was previously calculated function returns cached result
## if not - it will calculate inversed version and put it into cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting chached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
