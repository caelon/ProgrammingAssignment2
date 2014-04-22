## This set of code is for the Coursera course R Programming
## The purpose of the code is to cache the results of matrix invesion 
## calculations. Matrix operations can take a long time so this set of
## functions allows you to cache the results of one instead of having 
## to repeat every time.

## makeCacheMatrix creates a special matrix object that can cache its
## inverse. It has four functions:
##   1. set - set the value of the matrix
##   2. get - retrieve the value of the matrix
##   3. setsolve - set the value of the inverse matrix
##   4. getsolve - retrieve the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL 
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve ##re-assign s to solve function
    getsolve <- function() s
    list(set = set, get = get, ##makes list of the functions
         setsolve = setsolve,
         getsolve = getsolve)
}


## First checks to see if this has been solved once. If so, returns the 
## cached data. If not, inverts the matrix, stores the inversion for
## future use, adn returns the inversion.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) { ## this is the check whether it's been solved.
        message("getting cached data")
        return(s)  ## this return exits the function
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}