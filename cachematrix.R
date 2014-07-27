## These fuctions will cache the inverse of a matrix if it has already been computed

## This makes a special matrix which is capable of storing it's own inverse.
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





## This matrix will either calculate or retrieve the inverse of the original matrix, when given the output of the cachematrix function.

cacheinverse<- function(x, ...) {
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
        ## Return a matrix that is the inverse of 'x'
