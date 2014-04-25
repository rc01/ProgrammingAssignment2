## The purpose of the functions is two-fold:
## (i)  Creating a matrix with the ability of incorporating new values or
##      retrieving previously recorded values.
## (ii) Computing the inverse of that matrix or extracting its cached 
##      when previously stored.
## These two functions interact with each other.

## The "makeCacheMatrix" function set or retrieve matrix values. Also the 
## function gets or stores the matrix inverse.   
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                         
    set <- function(y) {                # set the value of a matrix object
        x <<- y                         
        m <<- NULL                       
    }
    get <- function() x                 # extract the value from the object
    setInverse <- function(Inverse) m <<- Inverse  # store the matrix inverse  
                                                   #value for future use
    getInverse <- function() m          # extract the matrix inverse value
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The "cacheSolve" function computes the matrix inverse in the case that this 
## is not already cached.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()                 # query the x matrix's cache         
    if(!is.null(m)) {                   # if there is a cache
        message("getting cached data") 
        return(m)                       # return the cache w/o computation
    }
    data <- x$get()                     # if there's no cache
    m <- solve(data, ...)               # matrix inversion computation here
    x$setInverse(m)                     # save the result back to x's cache
    m                                   # return the result
}
