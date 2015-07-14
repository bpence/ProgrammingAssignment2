################################################################################
## The following functions store a matrix inversion and retrieve it rather    ##
## than calculating it again.  Inversion of matrices with large dimensions is ##
## computationally intensive, and the following functions allow for the       ##
## conservation of computational resources.                                   ##
################################################################################


################################################################################
## This function generates a list which (1) stores a matrix value "set", (2)  ##
## retrieves the stored matrix value "get", (3) stores the inverse value of   ##
## the matrix "setinverse", and (4) retrieves the inverse value of the matrix ##
## "getinverse".                                                              ##
################################################################################

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) s <<- inv
    getinverse <- function() s
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

################################################################################
## This function calculates the inverse value of a matrix.  However, first it ##
## checks to see if the value was previously stored and, if so, retrieves     ##
## that value rather than calculating it anew.  This outcome is marked by the ##
## printed phrase "getting cached data" prior to the returned matrix inverse. ##
## If the value of the matrix inverse is not stored, the function calculates  ##
## the value and returns the answer.                                          ##
################################################################################

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setinverse(s)
    s
}

################################################################################
##                        HERE IS AN EXAMPLE RUN:                             ##
##                                                                            ##
## > x <- matrix(1:4,nrow=2,ncol=2)                                           ##
## > a <- makeCacheMatrix(x)                                                  ##
## > a$get()                                                                  ##
##     [,1] [,2]                                                              ##
## [1,]   1    3                                                              ##
## [2,]   2    4                                                              ##
##                                                                            ##
## > cacheSolve(a)                                                            ##
##     [,1] [,2]                                                              ##
## [1,]  -2  1.5                                                              ##
## [2,]   1 -0.5                                                              ##
##                                                                            ##
## > cacheSolve(a)                                                            ##
## getting cached data                                                        ##
##     [,1] [,2]                                                              ## 
## [1,]  -2  1.5                                                              ##
## [2,]   1 -0.5                                                              ##
##                                                                            ##
## The output can be verified to be correct by using the solve function:      ##
##                                                                            ##
## > solve(x)                                                                 ##
##     [,1] [,2]                                                              ##
## [1,]  -2  1.5                                                              ##
## [2,]   1 -0.5                                                              ##
##                                                                            ##
################################################################################



