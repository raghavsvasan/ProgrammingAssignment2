## Caching the Inverse of a Matrix. Since matrix computation is usually a costly operation, the results are cached and can be looked up again instead of recomputing 
## The below functions use the results if its already present else it calcuated the inverse value of a matrix 

## This function creates a special "matrix" object that can cache its inverse after its computed the first time.

makeCacheMatrix <- function(x = matrix()) {
 s <- NULL
 set <- function(y)
    {
    x <<- y
    s <<- NULL
    }
 get <- function() x
 setinv <- function(solve) s <<- solve
 getinv <- function() s
 list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}	


## This function calculates the inverse of the matrix returned by the above function makeCacheMatrix. If the calculation is already done, it retrieved the value from cache. 

cacheSolve <- function(x, ...) {
 s <- x$getinv()
 if(!is.null(s)) 
    {
    message("getting cached data")
    return(s)
    }
 data <- x$get()
 s <- solve(data, ...)
 x$setinv(s)
 s
}
