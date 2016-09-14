## Matrix inverse is usually costly computation. The following functions uses
## the lexical scoping in caching the computed inverse value and returns the 
## cached value.

## The firs function makeCacheMatrix() creates a vector of list containing function to
## set() - set the value of the vector
## get() - get the value of the vector
## setinverse() - set the inverse of the matrix argument
## getinverse() - get the inverse of the matrix argument

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
    	setinverse = setinverse,
    	getinverse = getinverse)
}


## The second function cacheSolve() computes the inverse of the matrix created
## with the above function. It first checks inverse been already computed and 
## if yes, fetch the values from cache and returns, otherwise computes the inverse
## and sets the same in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
