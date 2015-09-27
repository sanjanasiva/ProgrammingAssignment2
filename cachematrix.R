makeCacheMatrix <- function(x = matrix()) {
        ## x: a square invertible matrix
        
        inv = NULL
        set = function(y) {
                x <<- y 
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix()
        
        inv = x$getinv()
        
        # if inverse has already been calculated
        if (!is.null(inv)){ 
                message("getting cached data")
                return(inv)
        }
        
        # calculates the inverse 
        data = x$get()
        inv = solve(data, ...)
        
        # sets the value of the inverse
        x$setinv(inv)
        
        return(inv)
}
