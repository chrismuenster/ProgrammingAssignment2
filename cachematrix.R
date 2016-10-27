## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
##  makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
##  cachesolve() requires an argument that is returned by makeCacheMatrix() in order 
##  to retrieve the inverse from the cached value that is stored in the makeVector() object's environment.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, then cacheSolve 
## retrieves the inverse from the cache. If not, the inverse is calculated and cacheSolve 
## stores the inverse

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(i)
        i
}

