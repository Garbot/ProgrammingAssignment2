## Program to create a matrix object with caching functions, and to find
## the inverse of that matrix.

## makeCacheMatrix function creates a matrix object with functions for
##      -setting the matrix
##      -getting the matrix
##      -setting the inverse of the matrix
##      -getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        myInverse <- NULL
        set <- function(y){
                x <<- y
                myInverse <<- NULL
        }
        get <- function(){
                x
        }
        setInverse <- function(solve){
                myInverse <<- solve
        }
        getInverse <- function(){
                myInverse
        }
        #return list of functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)        
}

## The cacheSolve function checks to see if the inverse has already been calculated.
##      -If yes, returns the cached inverse.
##      -If not, calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## if Inverse has already been cached, return cached data and exit function
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## if inverse hasn't been cached, get data, compute the inverse, and cache the inverse.
        data  <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
