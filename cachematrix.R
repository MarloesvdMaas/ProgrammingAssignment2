## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                   ## set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                    ## get the value of the matrix
    setInv <- function(solve) m <<- solve  ## inverse the matrix
    getInv <- function() m                 ## get the inverse of the matrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## A vector (list) is now created that contains the function to inverse the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")    ## if m is in cache, it is not equal to null (!is.null(m)), then the message will be returned
        return(m)                         ## plus the value of m
    }
    data <- x$get()                       ## if m is not in cache, then the function x$get from the makeCacheMatrix function is returned
    m <- solve(data, ...)                 
    x$setInv(m)                           ## and the inverse matrix is calculated here
    m
}
