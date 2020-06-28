##Below are two functions that are used to create a special matrix 
##that stores a matrix and cache's its inverse.


## The first function, makeCacheMatrix creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function computes the inverse of the special "matrix" returned
## by the first makeCacheMatrix function. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data)
        x$setinverse(n)
        n
}


