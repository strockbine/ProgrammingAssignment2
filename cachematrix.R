## The following functions implement a matrix-like object that
## remembers both the matrix assigned to it and (once computed)
## the inverse of that matrix. A new matrix can be assigned to
## the object, at which point the old inverse is forgotten.
##
## Usage:
## m <- matrix(c(1, 2, 2, 1), 2, 2)   -- create a matrix
## mc <- makeCacheMatrix(m)           -- create a cached matrix
## cacheSolve(mc)                     -- compute the inverse of the matrix
##
## After the last operation above, subsequent calls to
## `cacheSolve(mc)` will return the previously computed inverse
## unless a new matrix is stored in `mc`.


## makeCacheMatrix
##
## Accepts a matrix as its only parameter and returns a list of
## functions to get and set the stored matrix and the stored
## inverse. Assumes that any matrix passed in has an inverse.
##
## The object returned by `makeCacheMatrix` is actually a list
## of functions (get, set, getsolve, and setsolve). The `set` and
## `setsolve` functions use the `<<-` operator to set the values
## of `x` (the original matrix) and `s` (the inverse) in the parent
## environment defined by makeCacheMatrix. R's lexical scoping
## allows the `get` and `getsolve` functions to find the values
## of `x` and `s` that are saved in makeCacheMatrix's environment.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## cacheSolve
##
## Accepts an object created by makeCacheMatrix and returns the
## inverse of the matrix.
##
## The `cacheSolve` function employs the functions in the list
## created by `makeCacheMatrix`, which is passed as the parameter
## `x`. It first calls `getsolve` to access the stored inverse.
## If that value is not NULL, it prints a message stating that it's
## using cached data, and then returns the inverse. Otherwise, it
## computes an inverse using R's `solve` function and stores
## that inverse using `setsolve`.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve();
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    m <- x$get()
    s <- solve(m, ...)
    x$setsolve(s)
    s
}