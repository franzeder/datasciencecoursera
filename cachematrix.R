## The makeCacheMatrix function creates a special matrix, which is really a list
## containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of inverse matrix
##      4. get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
        x <<- y
        i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated, then 
## the cachesolve retrieves the inverse from the cache and reports as messsage.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
