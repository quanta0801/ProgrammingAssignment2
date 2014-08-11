## These 2 functions seeks to create and store the inverse of a matrix within the cache
## This is very similar to the one to cache mean that is given in the example

## this function is the special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { #set function to create object
        x <<- y
        m <<- NULL
    }
    get <- function() x #get function to call the object
    setinverse <- function(inverse) m <<- inverse #to save the inverse within cache
    getinverse <- function() m #to call the inverse value
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function attempts to obtain the inverse of the matrix if it is not already in cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) { #check if inverse in cache
        #if it is within cache, obtain directly from cache
        message("getting cached data")
        return(m)
    }
    #if not in cache, solve for inverse and save in cache within the special matrix object
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}