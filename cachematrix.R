## Set of functions to create cache-matrix and solve the inverse

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## Usage:
##      makeCacheMatrix(X = matrix())
## Arguments:
##      X matrix for which cache-matrix will be created, default is empty matrix
## Methods:
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


## Returns cached inverse from cache-matrix created with makeCacheMatrix or solves
## inverse using solve(), stores it to cache-matrix and retruns it
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
