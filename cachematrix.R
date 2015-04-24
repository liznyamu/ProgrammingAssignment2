## below functions cache the inverse of a matrix
## as computation of matrix inversion can be costly

## function 'makeCacheMatrix' :
# create a special list of functions for a matrix param 'x'

makeCacheMatrix <- function(x = matrix()) {
        # create an object to store the inverse of 'x'
        # set it to NULL
        i <- NULL 
        
        # set cache new matrix value and reset inverse to NULL
        set <- function() {
                x <<- y
                i <<- NULL
        }
        
        #get matrix 'x'
        get <- function() x
        
        #cache inverse of matrix 'x'
        setInverse <- function(inverse) i <<- inverse
        
        #get inverse of 'x'
        getInverse <- function() i
        
        #return special list with above nested functions
        list(set <- set, get <- get,
             setInverse <- setInverse, getInverse <- getInverse)
        
        
}


## function 'cacheSolve'
# calculate and cache the inverse of a matrix

cacheSolve <- function(x, ...) {
        # check if inverse of matrix 'x' has been calculated/cached
        i <- x$getInverse()
        if(!is.na(i)){
                message("gettin cached inverse")
                return(i)
        }
        
        # else calculate and cache the inverse of the matrix
        # get the value of the matrix
        data <- x$get()
        # calculate the inverse (pass the argurments)
        i <- solve(data, ...)
        # cache the matrix
        x$setInverse(i)
        i
        
        
        ## Return a matrix that is the inverse of 'x'
}
