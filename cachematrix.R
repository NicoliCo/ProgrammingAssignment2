## Programming assignment 2

## Creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL

    set <- function(y){
        x <<- y
        m <<- NULL
    }

    get <- function() x

    setInverse <- function(inverseMatrix) m <<- inverseMatrix

    getInverse <- function() m

    list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)

}


## computes the inverse of the special matrix created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)){
            message("Getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
