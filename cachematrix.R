##  This function is able to cache the potentially time-consuming computation,
## inverse of a matrix. If the contents of a matrix are not changing, we can
## cache the value of the inverse matrix so that when we need it again, it
## can be looked up in the cache rather than recomputed.
## Usage:
##        y <- makeCacheMatrix(x=matrix())
##        myInverseX <- cacheSolve(y)
## Example:
##        x = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=3, ncol=4)
##        x
##        y <- makeCacheMatrix(x)
##        myInverseX <- cacheSolve(y)
##        myInverseX
## Prerequisite:
##       package "MASS"

## Prerequisite
## Install prerequisite package "MASS" if needed
install.packages("MASS")

## Include prerequisite package "MASS"
library(MASS)

## The first function, "makeCacheMatrix" creates a special "matrix", which is
## really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it "gets" the inverse matrix
## from the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache via
## the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinverse(m)
        m
}
