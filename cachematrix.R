## The function makeCacheMatrix creates a special "matrix" that is able to 
## cache its inverse. The function cacheSolve either computes the inverse
## of the special "matrix" from makeCacheMatrix, or retrieves the inverse 
## matrix if it has already been computed and cached.

## makeCacheMatrix creates a list containing a function to set the matrix,
## get the matrix, set the inverse of the matrix, and get the inverse of the
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}


## cacheSolve determines the inverse of the special "matrix" created by
## makeCacheMatrix. It first checks to see if the inverse has already been
## computed. If yes, the inverse is retrieved from the cache, and the
## computation is skipped. Otherwise, it computes the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
}
