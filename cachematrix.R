## Michelle Conway Wednesday June 22, 2016
## R Programming
## Programming Assignment 2

## makeCacheMatrix is a function that takes 1 optional matrix argument
## and returns a list of functions to set and get the matrix
## and get and set the inverse of the matrix

## cacheSolve is a function that computes the inverse of a matrix
## if it has not already been calculated


## makeCacheMatrix takes one optional matrix argument x
## it returns a list of four functions
## makeCacheMatrix$set sets the matrix to the value passed
## makeCacheMatrix$get returns the matrix
## makeCacheMatrix$setinv sets the inverse of the matrix to local variable inv
## makeCacheMatrix$getinv returns the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## it takes one argument, the list created by makeCacheMatrix
## it first checks to see if inverse is not null
## if the inverse is not null, it returns that inverse
## if the inverse is null, it gets the data using the get function of makeCacheMatrix
## it computes the inverse using the solve function
## it sets the inverse using the setinv function of makeCacheMatrix
## it returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
