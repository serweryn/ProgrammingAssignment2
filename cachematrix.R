## Functions to create cache-matrix and solve the inverse and cache it

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
##
## Usage:
##      makeCacheMatrix(X = matrix())
## Arguments:
##      X matrix for which cache-matrix will be created, default is empty matrix
## Methods of this special "matrix":
##      set(X) sets new matrix for this cache-matrix
##      get() gets matrix of this cache-matrix
##      setInverse(INV) sets inverse which will be cached in this cache-matrix
##      getInverse() gets cached inverse or NULL if it was not set

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) inv <<- inverse

    getInverse <- function() inv

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
##
## Usage:
##      cacheSolve(X, ...)
## Arguments:
##      X cache-matrix created with makeCacheMatrix
##      ... optional additional parameters to solve()

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()

    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)

    inv
}
