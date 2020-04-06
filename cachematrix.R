## Function tasks:
## makeCacheMatrix: stores matrix to be inverted, stores any cached inversions
## cacheSolve: if no stored inversion, computes inverted matrix; in subsequent calls, retrieves cached inversion

## makeCacheMatrix function:
## Takes matrix as argument, returns list of functions:
##      1. setmat: sets matrix to be inverted; resets previously stored inversions
##      2. getmat: retrieves stored matrix to be inverted
##      3. setinv: stores inverted matrix (calculated via cacheSolve) into cache
##      4. getinv: retrieves stored inverted matrix from cache

## Other objects in this environment:
##      1. x is the matrix to be inverted
##      2. inversion is the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        setmat <- function(input){
                x <<- input
                inversion <<- NULL
        }
        getmat <- function() {
                x
        }
        setinv <- function(y){
                inversion <<- y
        }
        #Y is not previously defined here; solveMatrix will have a line to store its output as Y
        getinv <- function() inversion
        
        list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## cacheSolve function:
## Checks if an inverted matrix has been stored in makeCacheMatrix.object by checking value of inversion.object
##      If so, retrieves that matrix
##      If not, computes inverted matrix and stores via setinv() above

cacheSolve <- function(x, ...) {
        inversion <- x$getinv()           #Retrieves value of cached inversion
        if(is.null(inversion) == TRUE) {  #Checks to see if inversion has been reset
                y <- solve(x$getmat())    #If reset, calculates inversion and stores in cache
                x$setinv(y)
        } else {                          #If not reset, retrieves inversion
                print("retrieving cached matrix")
                return(inversion)
        }
}
