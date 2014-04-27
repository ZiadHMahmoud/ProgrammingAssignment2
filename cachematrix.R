## These functions are used to create an matrix object that can cache 
## it's inverse to avoid recacluation in case of bigger matrices who 
## need high computing time to calculate the inverse


## this function is used to create an object that will hold a matrix with
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## calculate the inverse of a matrix create by the makeCacheMatrix 
##funciton. if the inverse was calculated before it will be retrived 
##from cache otherwise it will be calculated and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
