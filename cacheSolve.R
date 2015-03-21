cacheSolve <- function(x, ...) {
xInv <- x$getinverse()
if (!is.null(xInv)) {
message("getting cached inverse matrix")
return(xInv)
} else {
xInv <- solve(x$get())
x$setinverse(xInv)
return(xInv)
}
}