## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
		 setinv = setinv,
		 getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	xinv <- x$getinv()
	if(!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
	}
	xmat <- x$get()
	xinv <- solve(xmat)
	x$setinv(xinv)
	xinv
}
