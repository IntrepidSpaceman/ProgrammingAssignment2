## This pair of functions permits the calculation of the inverse to be calculated
## only once per matrix. Once the value is calculated, it is cached in memory, and
## for subsequent calculations, this value is just retrieved instead of calculated again

## This function takes as input any matrix, and returns a representation in the form of a list
## The matrix is then represented not only by its values, but also by its inverse.
## These attributes are accessible via the get and set inner functions
## Notice that changing the matrix values, automatically resets its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## when first creating the matrix the inverse is not yet calculated, so inv is null
        inv <- NULL
        ## when setting a value for the matrix this as to substitute the original 'x' value and reset the inverse value
        set <- function(A) {
                x <<- A
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        ## the list is a representation of the supplied matrix along with its inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function should take as input the output of the makeCacheMatrix function
## It uses the get and set methods associated with the matrix to check if the calculation is needed
## If not, then the function calculates the inverse, and associates this value with the matrix

cacheSolve <- function(x, ...) {
        ## first the inverse value in the matrix representation is retrieved
        inv <- x$getinv()
        ## if it as already been calculated, then it is just returned
        if (!is.null(inv)) {
                message("Retriving cached data")
                return(inv)
        }
        ##Otherwise it is calculated, associated with the matrix representation and returned
        M <- x$get()
        inv <- solve(M, ...)
        x$setinv(inv)
        inv
}
