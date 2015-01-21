## makeCacheMatrix creates a list with 4 functions to (1) set the value of a matrix,
## (2) get the value of a matrix, (3) set the inverse of a matrix, and
## (4) get the inverse of a matrix. The matrix must be a square, invertible matrix.

## cacheSolve takes the returned value of makeCacheMatrix to calculate the inverse
## of a matrix and then set the inverse in cache via the setInverse function. 
## But, if the inverse has already been calculated, cacheSolve retrieves the 
## inverse matrix via the getInverse function. 


## makeCacheMatrix stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function () x
        
        setInverse <- function(solve) inverse <<- solve
        
        getInverse <- function () inverse
        
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'.
## 'A' is an argument where you pass the desired matrix to cacheSolve
## to ensure that the matrix hasn't changed from makeCacheMatrix. The default is set to
## the stored matrix.


cacheSolve <- function(x, A = x$get(), ...) {
        inverse <- x$getInverse()
        matrix <- x$get()
        
        equalMatrix <- function(x, y) {
                dim(x) == dim(y) && all(x == y)
        }
        
        equal <- equalMatrix(A, matrix)
        
        ## if there is a cached inverse of the matrix AND
        ## the specified matrix in the function argument is equal to the stored matrix,
        ## then the cached inverse matrix is being retrieved.
        if (!is.null(inverse) && equal) {
                message("getting cached data")
                return(inverse)
        }
        
        ## if there is a cached inverse of the matrix, 
        ## but the specified matrix and the stored matrix aren't equal,
        ## then the cached inverse is cleared, the stored matrix is updated, and
        ## a new inverse of the matrix is calculated and set.
        else if (!is.null(inverse) && !equal) {
                message("updating matrix, cache cleared")
                x$set(A)
                inverse <- solve(A, ...)
                x$setInverse(inverse)
                inverse        
        }
        
        ## if there isn't a cached inverse of the matrix,
        ## then the inverse is calculated and set.
        else {
                inverse <- solve(matrix, ...)
                x$setInverse(inverse)
                inverse 
        }
        
}
