## EXERCISE: Caching the Inverse of a Matrix
##
## Creator: tatarczak@gmail.com
## Date: 2016-09-02
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 

## Below you can find a pair of functions that are used to create 
## a special "matrix" object and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y 
                i <<- NULL
        }
        
        get <- function() {
                return(x)
        }
        
        setInverse <- function(inv) {
                i <<-inv
        }
        
        getInverse <- function() {
                return(i)
        }
        
        list(set = set
             ,get = get
             ,setInverse = setInverse
             ,getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getInverse()
        
        if (!is.null(i)) {
                message("Getting cached data!!!")
                return(i)
        }
        
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInverse(i)
        i
}