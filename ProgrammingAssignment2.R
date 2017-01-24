## The `makeCacheMatrix` function creates a special matrix object from a given
## one capable of caching its inverse. The returned object offers getters and
## setters for both the matrix and its inverse.



makeCacheMatrix <- function(x = matrix()) {
##The `makeCacheMatrix` function creates a special matrix object from a given
## one capable of caching its inverse. The returned object offers getters and
## setters for both the matrix and its inverse.
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The `cacheSolve` function takes an invertible matrix created using the
## aforementioned `makeCacheMatrix` function and returns its inverse created
## with the same matrix factory function. The `cacheSolve` function
## additionally takes other optional arguments that are passed to the inner
## call to the `solve` function used to compute the inverse.       
        
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
        
        
