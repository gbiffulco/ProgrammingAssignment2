## makeCacheMatrix creates a matrix object then cacheSolve
## calculates de inverse of such objetc.

## makeCacheMatrix will find the inverse matrix in the cache
## and return it. If makeCacheMatrix cannot find it, 
## it will calculate it.

makeCacheMatrix <- function(x = matrix()) {
        inv_x < NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is a function taht returns the inverse of a matrix
## created with the makeCacheMatrix function.

## If available, the function cacheSolve will retrieve the 
## cached inverse. If unavaileble, it computes, caches and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
                message("getting inverse matrix...")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}
