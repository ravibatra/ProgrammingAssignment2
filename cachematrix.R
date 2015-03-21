## makeCacheMatrix creates a special matrix object, and then cacheSolve calculates the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
xInv <- NULL
set <- function(y) {
x <<- y
xInv  <<- NULL
}
get <- function() x
setinverse<- function(inverse) xInv <<-inverse
getinverse <- function() xInv
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix A created with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
xInv <- x$getinverse()
if (!is.null(xInv)) {
	return(xInv)
} else {
	xInv <- solve(x$get())
	x$setinverse(xInv)
	return(xInv)
	}
}
