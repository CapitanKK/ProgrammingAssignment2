makeCacheMatrix <- function(m = matrix()) {
        ## creates a special "matrix", which is really a list containing
        ## a function to
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of the inverse
        ## 4. get the value of the inverse
        cacheInverse <- NULL
        set <- function(newMatrix) {
                m <<- newMatrix
                ## value changed, so cacheInverse is no longer valid
                cacheInverse <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) cacheInverse <<- inverse
        getInverse <- function() cacheInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(m, ...) {
        ## calculates the inverse of the special "matrix" created with the
        ## above function. However, it first checks to see if the inverse has
        ## already been calculated. If so, it gets the inverse from the cache
        ## and skips the computation. Otherwise, it calculates the inverse of
        ## the data and sets the value of the inverse in the cache via the
        ## setInverse function.
        inverse <- m$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                ## we have cache of inverse, so just return it
                return(inverse)
        }
        data <- m$get()
        ## calculate inverse of matrix
        inverse <- solve(data, ...)
        ## save inverse in cache of special "matrix"
        m$setInverse(inverse)
        inverse
}
