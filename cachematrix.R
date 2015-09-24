## Caching the Inverse of a Matrix
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
##
## Example:
## A<-matrix(c(1,3,2,1),nrow=2,ncol=2)
## cA<-makeCacheMatrix(A)
## solveCache(cA)
## cA$get()
## A[1,2]<-3
## cA$set(A)
## solveCache(cA)
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m		
}
