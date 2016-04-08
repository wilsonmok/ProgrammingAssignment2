## Matrix inversion is usually a costly computation and there may be some 
##   benefit to caching the inverse of a matrix rather than computing it 
##   repeatedly 
## 
## makeCacheMatrix: This function creates a special "matrix" object that 
##                  can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" 
##             object. If the inverse has already been calculated and not 
##             changed, it will return the inverse from the cache.


## This function creates a special "matrix" object that can cache its 
##    inverse.
makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
    
    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setsolve <- function(yinverse) {
        xinverse <<- yinverse
    }
    
    getsolve <- function() {
        xinverse
    }
    
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" object. 
## If the inverse has already been calculated and not changed, it will
##   return the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve();
    
    if (!is.null(s)) {
        message("getting cache data") 
        return(s)
    }
    else {
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        return(s)
    }
}
