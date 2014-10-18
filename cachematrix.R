## This set of functions provides functionality for finding the inverse of 
## a matrix using solve(). 
## The code assumes that the supplied matrix is invertible.
## The functions allow the user to leverage caching of the results of solve()
##
## Usage Example:
##   matrixToBeInverted <- matrix(data = 1:4, nrow = 2, ncol = 2)
##   usefulList <- makeCacheMatrix(matrixToBeInverted)
##   cacheSolve(usefulList)
##   cacheSolve(usefulList) ## Note that the cache is used on the second call


## This function takes a matrix as a formal parameter and returns 
## a list of functions for getting/setting various environment variables

makeCacheMatrix <- function(x = matrix()) {
        StoredMatrixInverse <- NULL
        set <- function(y) {
                x <<- y
                StoredMatrixInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(theInverse) StoredMatrixInverse <<- theInverse
        getInverse <- function() StoredMatrixInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function finds the inverse of a matrix; on subsequent
## calls with the same input, the answer is retrieved from a cache of results

cacheSolve <- function(x, ...) {
        StoredMatrixInverse <- x$getInverse()
        if(!is.null(StoredMatrixInverse)) {
                message("getting cached data")
                return(StoredMatrixInverse)
        }
        data <- x$get()
        StoredMatrixInverse <- solve(data, ...)
        x$setInverse(StoredMatrixInverse)
        StoredMatrixInverse
}
