## Create the inverse matrix for a square matrix
## Since inverse matrix is expensive opperation
##     the calculated iverse matrix is cached in the memory
##     so that is that it can be accessed from the cache 
##     to avoid recalculate


## This function creates the inverse matrix using matrix 
##  operation solve and store into cache / memory environment

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im  <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) im <<- solve
        getsolve <- function() im
        list(set = set, get = get,
             setsolve = setsolve ,
             getsolve = getsolve )
}



## This function retrive the cached inversed matrix

cacheSolve <- function(x, ...) {
        im <- x$solve()
        if(!is.null(im)) {
                message("getting cached inversed matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setsolve(im)
        im        
}
