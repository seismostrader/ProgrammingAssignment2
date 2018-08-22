## The functions "makeCacheMatrix" and "cacheSolve" compute the inverse of an input matrix and store the result. If 
## the next input matrix is identical to the previous one, the the function "cacheSolve" will simply retrieve the 
## inverse of the previous (identical) matrix instead of recomputing the inverse, which would be more computationally
## intensive.

## Creates a list of accessor and mutator functions used to get and set the input matrix and its inverse. Also 
## defines the input matrix "x" and an inverse value "inv" in the function environment. If a new matrix is created,
## "inv" is initialized to NULL.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize inverse to NULL, because a new matrix is created
    set <- function(y) {  # set the value of x (the input matrix)
        x <<- y  # set the value of x in the parent environment (the function containing this function)
        inv <<- NULL  # set the inverse to NULL because x was reset and therefore inv needs to be recalculated
    }
    get <- function() x  # get the value of x from the makeCacheMatrix object
    setinv <- function(inverse) inv <<- inverse  # set the inverse of the matrix x by calling function from makeCacheMatrix object
    getinv <- function() inv  # get inverse of input matrix: if previous x value is same as current, a value will be cached, if not, inv = NULL
    list(set = set, get = get, setinv = setinv, getinv = getinv)  # name functions in list so they can be called using $ operator
}


## This function takes a makeCacheMatrix object and calculates the inverse if x is a new value, or retrieves 
## the cached inverse value if x is the same as previously.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()  # retrieve cached inverse value (if x was reset, inv = NULL)
    if(!is.null(inv)) {  # There is a cached inverse value != NULL, because x is the same value as previously
        message("getting cached data")
        return(inv)  # return cached inverse of x
    }
    # if x is a new matrix and inv was reset to NULL:
    data <- x$get()  # get value of x using the makeCacheMatrix object (which is stored in the global environment)
    inv <- solve(data, ...)  # compute the inverse of x
    x$setinv(inv)  # store the inverse of the current value of x
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
