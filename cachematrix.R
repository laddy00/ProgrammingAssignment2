#Programming Assignment 2: Lexical Scoping: 
#Author: Chris Canaday
#File: cachematrix.R
#Date: 2015/05/22
# makeCacheMatrix takes a matrix and creates a special "matrix" object that can cache its inverse
# cacheSolve calculates matrix inverse if not already cached. if already cached, returns cached value


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                  #sets the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                   #Returns the Matrix
        setinverse <- function(inv) m <<- inv #Sets inverse matrix
        getinverse <- function() m            #Returns inverse Matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

