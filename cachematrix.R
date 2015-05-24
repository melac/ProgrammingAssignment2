#These functions can be used to produce the inverse of a square matrix and cache it
##Caching the inverse of your matrix allows you to call it later without requiring R to perform the function repeatedly, which will save you time

##The first function, called makeCacheMatrix, creates a special maatrix that can cache its inverse
#After running the script for the function, you can use it by typing makeCacheMatrix(a), where a is your square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


##The second function, called cacheSolve, retrieves the inverse of your matrix from the cache if it has already been calculated,
## or if the inverse has not yet been calculated, it will calculate the inverse of your matrix using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
