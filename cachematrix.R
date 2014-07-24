## This pair of functions permits the calculation of the inverse to be calculated
## only once per matrix. Once the value is calculated, it is cached in memory, and
## for subsequent calculations, this value is just retrieved instead of calculated again

## This function takes as input any matrix, and returns a representation in the form of a list
## The matrix is then represented not only by its values, but also by its inverse.
## These attributes are accessible via the get and set inner functions
## Notice that changing the matrix values, automatically resets its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(A) {
                x <<- A
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function should take as input the output of the makeCacheMatrix function
## It uses the get and set methods associated with the matrix to check if the calculation is needed
## If not, then the function calculates the inverse, and associates this value with the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("Retriving cached data")
                return(inv)
        }
        M <- x$get()
        inv <- solve(M, ...)
        x$setinv(inv)
        inv
}
