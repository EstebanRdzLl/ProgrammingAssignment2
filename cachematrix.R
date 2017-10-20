## This script contains 2 functions, one that generates a list of functions for
## the second function to use, which actually calcullates the inverse of the 
## data input

## This function initializes both the original matrix and the inverse as global 
## (parent) values, then creates a set of functions which work as setters and 
## getters for future referencing and finally makes a named list of this 
## functions

makeCacheMatrix <- function(M = matrix()) {
        # dM<-dim(M)
        inv <- NULL
        set <- function(N) {
                M <<- N
                inv <<- NULL
        }
        get <- function() M
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This functions takes an object of type "makeCacheMatrix" and uses this object 
## to access the functions in it (see previous function) to check for a possible
## value of the inverse matrix. If it exists, it takes the value located in the 
## parent directory and if it doesn't it computes it and save it to the memory.

cacheSolve <- function(mkMat, ...) {
        inv <- mkMat$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        M <- mkMat$get()
        inv <- solve(M)
        mkMat$setinv(inv)
        inv
}