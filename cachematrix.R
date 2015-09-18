## The following functions works together to create a invertible matrix
## and make the inverse of matrix available in the cache environment

## Assumption for creating a invertible matrix is matrix supplied is square matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        ## function for caching the matrix
        set <- function(y) {
                 x <<- y
                 inverse <<- NULL
        }

        ## function for retrieving the matrix
        get <- function() x

        ## function for caching the inverse matrix
        setinversematrix <- function(inversematrix) inverse <<- inversematrix

        ## function for retrieving the inverse matrix
        getinversematrix <- function() inverse

        ## returning the list of functions
        list(set=set,get=get,
             setinversematrix=setinversematrix,
             getinversematrix=getinversematrix)
}



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinversematrix()
        ## checking whether inverse of a matrix is available in cache
        ## if available then inverse matrix is returned from cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        ## solve will return inverse of matrix by considering identical matrix as second argument
        inverse <- solve(data)
        ## setting the inverse of matrix in cache
        x$setinversematrix(inverse)
        ## returning the inverse matrix
        inverse
}