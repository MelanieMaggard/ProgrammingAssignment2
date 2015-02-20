## These two functions create a matrix object, cache its inverse,
## and compute the inverse.

## The following function creates a special "matrix" object that can cache its inverse. 
## Note: Follow these steps:  (1) Run functions below. (2) Assign makeCacheMatrix(x) to a value.  
## (3) Use $set after value to create or set the matrix. Example: a$set(matrix(c(4,2,7,6),ncol=2))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve)  m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it
## it will return the inverse from the cache.  Note:  Simply run the function for the matrix set above.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }
       matrix <- x$get()
       m <- solve(matrix, ...)
       x$setinverse(m)
       m
}
