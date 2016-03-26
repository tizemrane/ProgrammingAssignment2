## The script contains two functions to create and use a special cached matrix

## This function, makeCacheMatrix creates a cached matrix and allocate cached memory space for the inverse 

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mi <<- inv
        getinv <- function() mi
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function checks to see if the inverse has been calculated
## If so, it gets the inversed matrix  from the cache 
## Otherwise, it calculates the iverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        mat <- x$get()
        mi <- solve(mat, ...)
        x$setinv(mi)
        mi
        
}