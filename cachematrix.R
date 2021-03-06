## create a matrix object
## methods:
##      set - sets matrix
##      get - return matrix
##      setinverse - sets matrix inverse
##      getinverse - return matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) x_inv <<-inv
    
    getinverse <- function() x_inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## Return a inverse of matrix
## if it's cached returns already calculated (cached) inverse

cacheSolve <- function(x, ...) {

    x_inv <- x$getinverse()
    
    if (!is.null(x_inv)) {
    
        message("getting cached inverse matrix")
        return(x_inv)
    } 
    
    x_inv <- solve(x$get())
    x$setinverse(x_inv)
    
    return(x_inv)
    
}
