## Create a vector list of 4 functions 
## set function sets the value of the matrix
## get function gets the value of the matrix that was saved
## setinv sets the inverse matrix
## getinv gets the inverse matrix that was saved

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    ## result is a list with 4 functions
}

## cacheSolve checks first to see if the inverse is in memory if it is, the retreived matrix is returned with a message
## if there is no inverse in memory, the inverse is calculated and saved

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv ## the inverse is returned
}
