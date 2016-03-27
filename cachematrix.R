## The two functions ""makeCacheMatrix" and "cacheSolve" work together to produce the inverse of a matrix.
##
## The first call should be to "makeCacheMatrix" with the square matrix to be inverted.
##
## Subsequent calls should be to "cacheSolve": the first time that this function is called it will actually
## calculate the inverse matrix, and store it in the global environment... subsequent calls to "cacheSolve" will
## return the previously calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
## This function creates a list of four functions which hangle the storage and retrieval of a matrix and its inverse.
## The square matrix to be inverted should be supplied as the argument to this function. The result should be assigned to
## a user-defined variable.
    
        m <- NULL
        
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
## The object returned from "makeCacheMatrix Matrix" (best stored as a variable) should be supplied as the argument to this
## function. The first call to this function will calculate and then return the inverse of the square matrix. Subsequent calls
## will return the cached inverse as was calculated on the first call, but without any recalculation.
    
        m <- x$getinverse()
        
        if(!is.null(m)) return(m)
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    
}
