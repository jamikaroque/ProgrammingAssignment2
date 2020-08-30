## These set of functions is used to initialize and store matrices
## where their coressponding inverse can be cached i.e. stored and retrieved

## This function creates a list of functions that is used to store
## and obtain a matrix and its corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #storage for inverse of x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is used to retrieve the cached inverse of a matrix x.
## If the inverse is not yet cached, it calculates the inverse and caches it.
## x is the function list created through makeCacheMatrix that corresponds to a matrix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
