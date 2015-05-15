## Functions that cache the inverse of a matrix.
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## date: 15 May 2015
        ## author: dellaop, script based on Cousera's
              # R Programming Course, Assignment 2
        ## Input: invertible matrix
        ## Output: list containing a function to  
              # set the value of the matrix
              # get the value of the matrix
              # set the value of the inverse matrix
              # get the value of the inverse matrix
  
        ## --------------------------------
        
        m <- NULL
        ## <<- operator assigns a value to an object 
              # in an environment that is different from 
              # the current environment
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        ## ... according to lexical scoping, 'x' is that
            #  defined in the the calling environment (the
            #  'parent frame'), i.e. the input matrix
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
		# returned by function 'makeCacheMatrix'. 
cacheSolve <- function(x, ...) {
        ## date: 15 May 2015
        ## author: dellaop, script based on Cousera's
          # R Programming Course, Assignment 2
        ## Input: special object that stores an
          # invertible matrix and cache's its inverse,
          # created by the function makeCacheMatrix
        ## Output: inverted matrix
        
        ## --------------------------------
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ##  If the inverse has already been calculated
        ##  (and the matrix has not changed), 
        ## then the ''cachesolve' should retrieve 
        ## the inverse from the cache.
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
