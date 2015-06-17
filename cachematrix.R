## https://github.com/johnjmackle/ProgrammingAssignment2.git

## makeCacheMatrix creates a matrix that is a list that gets and sets the value of the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve gets the matrix of the inverse of makeCacheMatrix and skips the calculation if already completed. Prevents infinite loop.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
