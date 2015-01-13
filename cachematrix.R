## Put comments here that give an overall description of what your
## functions do
# Create a "Special matrix" that can store Inverted matrix in cache.
# plus a function that can return Inv matrix stored in cache if exist
# o/w computes it. Purpose: To avoid many computations of the same Inv Matrix.

## Write a short comment describing this function
# Define functions that allow init matrix, init inv matrix
# and also get matrix and get inv matrix.
# Not that if matrix is init then inv matrix in cache is reset.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinv <- function(solve) inv <<- solve
    getinv <- function()inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# Check if inv matrix is in cache to avoid computation (case identified by message)
# If not then inv matrix is calculated, stored in cache and return.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting from cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
