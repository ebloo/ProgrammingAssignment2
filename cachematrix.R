####################################################################################
##
## cacheMatrix.R
##
## Contains a framework for computing and caching the inverse of a matrix,
## leveraging scoping rules of R for efficiency.
##
## Can be invoked as follows:
## mymatrix <- matrix(rnorm(4), 2, 2)
## m <- makeCacheMatrix(x = mymatrix)
## cacheSolve(m)    # note that subsequent calls to cacheSolve hit the cache
## 
####################################################################################


####################################################################################
## This function creates a special "matrix", which essentially consists of a list 
## containing 4 functions:
##   1. set, which sets the value of the matrix
##   2. get, which simply returns the matrix
##   3. setinverse, which sets the inverse
##   4. getinverse, which returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse to NULL
    inverse <- NULL
    
    # Define the 4 functions described above (see comment)
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    # Return the list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


####################################################################################
## This function returns the inverse of the special "matrix" passed as a parameter
## First, it checks whether the inverse has already been cached. If so, it returns
## from cache. Otherwise, it computes the inverse, caches it for future use, and
## returns it.

cacheSolve <- function(x, ...) {
    # Attempt to get the inverse from cache
    inverse <- x$getinverse()
    
    # If already cached, simply return the inverse from cache
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # Otherwise, compute the inverse, cache it, and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
