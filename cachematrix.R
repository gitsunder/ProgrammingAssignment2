## Put comments here that give an overall description of what your
## functions do

## Short comment describing this function:
## This function to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## Initialize the inverse flag
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
 
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse flag
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Short comment describing this function
## Compute the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then "cachesolve" retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  	
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix 
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse 
    x$setInverse(m)

    ## Return the matrix
    m
}
