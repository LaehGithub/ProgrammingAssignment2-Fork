## These functions give the inverse of a matrix and cache previous results

## This function creates a special matrix object that can set and get the values
## of the Matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function calculates the inverse of the special matrix, but it first checks
## if the inverse has already been calculate

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i ## Return a matrix that is the inverse of 'x'
}

