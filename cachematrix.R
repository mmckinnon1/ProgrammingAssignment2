
## These two functions together cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## set is used to set the value of the matrix.
## get is used to get the value of the matrix.
## setmatrix is used to set the value of the inverse matrix.
## getmatrix is used to get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        minvert <- NULL
        set <- function(y){
                x <<- Y
                minvert <<- NULL
        }
        get <- function()x
        setmatrix <- function(inverse) minvert <<- inverse
        getmatrix <- function() minvert
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}

## cacheSolve fuction computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If inverse has already been calculated ( and matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

## For this function the supplied matrix will need to be invertible.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minvert <- x$getmatrix()
        if (!is.null(minvert)){
                message("getting cached data")
                return(minvert)
        }
        data <- x$get()
        minvert <- solve(data,...)
        x$setmatrix(minvert)
        minvert 
}
