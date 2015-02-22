## Because matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute it
## repeatedly, here are the functions that cache the inverse of a matrix.

## This function creates a special matrix with functions
## to set and get the value of the matrix, and
## to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse
    inv <- NULL

    ## Set value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## Get value
    get <- function() { x }

    ## Set inverse
    setInv <- function(solved) { inv <<- solved }

    ## Get inverse
    getInv <- function() { inv }

    ## Return the list of functions
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)

}


## Calculates the inverse of a matrix created with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()

    ## Check if the inverse already calculated
    ## If so, it gets the inverse from the cache and skips the computation
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
