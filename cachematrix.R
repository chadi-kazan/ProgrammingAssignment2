## The two proposed function implement the possibility to cache the inverse of a matrix
## by creating a new "special" matrix object makeCacheMatrix and an inverting function.
## cacheSolve

## This function takes a special matrix object and 
## creates in the a global environment  environment a variable x initialized
## with the passed matrix and creates another variable invMAt to NULL. 
## This invMAt variable will contain a cached version of the inverse of x. 
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMat <<- inverse
    getInverse <- function() invMat
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes an argument object of class makeCacheMatrix
## When called the first time, this function test the cached inverse
## invMAt; if NULL, compute the inverse of the cached matrix and
## set invMAt with this inverse.Afterwards, the cached inverse 
## will be returned if any instead of recomputing it.
##
##Example
## cm <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## cacheSolve(cm)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## Second call return the cached inverse
## cacheSolve(cm)
## getting cached inverse
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x = makeCacheMatrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInverse()
    if(!is.null(invMat)) {
        message("getting cached inverse")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data)
    x$setInverse(invMat)
    invMat    
}


