## Caching the Inverse of a Matrix:
## Matrix inversion can be time and space consuming calculation so its useful 
## to cache the inverse of a matrix rather than computing it repeatedly.
## Here are presented two functions that create a special object by storing 
## a matrix and caching its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
        set <- function (y) {
               x <<- y
               i <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then this should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        i <- x$getInverse ()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setInverse(i)
        i
}


## Testing my functions
mm <- makeCacheMatrix(matrix(1:4, 2, 2))
mm$get()
mm$getInverse()
cacheSolve(mm)
cacheSolve(mm)
