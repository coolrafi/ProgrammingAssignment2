## Below functions are used to compute the inverse of a square matrix using solve function.
## The first function caches the matrix while second inverses it.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y                                     ## set the value of matrix
        m <<- NULL
    }
    getMatrix <- function() x                       ## return matrix
    setInverse <- function(inverse) m <<- inverse   ## cache inverted matrix
    getInverse <- function() m                      ## return inverted matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()  ## check if inverted matrix already in cache (retrieve inverted matrix)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)        ## return inverted matrix read from cache
    }
    data <- x$getMatrix() ## get value of matrix
    m <- solve(data, ...) ## calculate inverted matrix
    x$setInverse(m)       ## save inverted matrix in cache
    m
}
