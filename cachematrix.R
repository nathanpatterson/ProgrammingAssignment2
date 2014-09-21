## These functions perform two tasks - the first creates and caches a matrix, 
## the second returns the inverse of that matrix, but only if the inverse doesn't already exist.
## If it does, the second function returns the cached value instead.

## This function will take a matrix as an input, defines functions to set and retrieve the matrix, 
## as well as set and retrieve the inverse of the matrix, and returns a list containing them.

makeCacheMatrix <- function(x = matrix()) {
	mat1 <- NULL
	set <- function(y) {
		x <<- as.matrix(y)
		mat1 <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) mat1 <<- solve
	getmatrix <- function() mat1
	list(set=set, get=get,
		setmatrix=setmatrix,
		getmatrix=getmatrix)
}


## This function will return the inverse of the matrix input, but checks first to see if the inverse
## has been calculated; if so, and the matrix hasn't changed, it returns the cached value.  Otherwise,
## the updated inverse is calculated and returned.

cacheSolve <- function(x=matrix(), ...) {
	mat1 <- x$getmatrix()
	if(!is.null(mat1)){
		message("inverse already calculated, getting cached data...")
		return(mat1)
	}
	mat2 <- x$get()
	mat1 <- solve(mat2, ...)
	x$setmatrix(mat1)
	mat1
}