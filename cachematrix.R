## Two functions that are used to create a special object that stores a matrix
## and cache's its inverse

## Creates a list containing functions that set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}

## Calculates the inverse of the matrix, first checking to see if it has already
## been calculated

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
