## Below methods will cache the inverse of matrix 'x' instead of 
## recomputing it repeatedly

## This function creates a special "matrix" object - that can
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## invs / inverse local variable 
        invs <- NULL
        
        ## set newX as new value of x and reset invs to undefined on cache env
        setX <- function(newX){
                x <<- newX
               invs <<- NULL 
        }
        
        ## get value of x
        getX <- function() x
        
        ## set newInvs as new value of invs on cache env
        setInverse <- function(newInvs) invs <<- newInvs
        
        ## get value of invs
        getInverse <- function() invs
        
        ## create a special list with list of all methods
        list(setX = setX, getX = getX, 
             setInverse = setInverse, getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix"
##      returned by makeCacheMatrix
cacheSolve <- function(cacheInvs,x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- cacheInvs$getInverse()
        cachedX <- cacheInvs$getX()
        
        ## if cached invs is not null and matrix has not changed
        ## return cached invs
        if(!is.null(invs) & matequal(cachedX, x)){
             return  invs
        }
        else {
                ## compute new inverse of x and cache x and its inverse
                cacheInvs$setX(x) 
                cacheInvs$setInverse(solve(x))   
        }
        
        cacheInvs$getInverse()
}

## function copied from 
## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y)
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
