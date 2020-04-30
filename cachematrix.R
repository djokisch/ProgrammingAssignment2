## makeCacheMatrix creates a 'matrix' object that can cache its inverse
## cacheSolve computes the inverse of a ''matrix', but only if it cannot be retrieved
## from the cache

## makeCacheMatrix returns a list of 4 functions (set, get, setinv, getinv)
## it essentially creates an object whose functions can be called and returned
## to the parent environment

makeCacheMatrix <- function(x = numeric()) {
    ## assuming that x is an invertible (square) matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calls getinv from the makeCacheMatrix function
## if the result is not NULL, it retrieves the cached inverse and returns it
## otherwise it calculates an inverse, sets it in makeCacheMatrix, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
