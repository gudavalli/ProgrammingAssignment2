## The following pair of functions implement caching mechanism for computation
## of inverse of a matrix. The inverse is cached the first time it is computed.
## All consequent requests for inverse of the same matrix are fulfilled by
## retrieving the inverse from the cache, rather than computing it again.
## To use the functionality, first get a cached version of input square matrix,
## and then use cacheSolve (not solve) on the cached version of matrix.


## makeCacheMatrxi function accepts a SQUARE matrix as input and implements
## caching functionality.  Non-square matrices cause fatal error. It returns
## an object that implements setter and getter functions for the matrix, and
## setter and getter functions for the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	## Initialize inverse
	inv <- NULL
	## Setter function for cached version of x
	set <- function(y) {
			x <<- y
			inv <<- NULL
		}
	## Getter function for cached version of x
	get <- function() x
	## Setter function for caching inverse of x
	setinv <- function(inverse) inv <<- inverse
	## Getter function for retrieving cached version of inverse of x
	getinv <- function() inv
	## return cached version of x
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## cacheSolve implements the cached version of inverse computation of a SQUARE
## matrix. This function can only be used with the return object of
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
	## Retrive inverse of x from cache
	xinv <- x$getinv()
	## If the inverse has previously been computed, xinv would not be null so
	## return xinv
	if(!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
	}
	## If the inverse has previously NOT been computed, compute inverse of x
	## using solve()
	xmat <- x$get()
	xinv <- solve(xmat)
	## Cache the computed inverse
	x$setinv(xinv)
    ## Return a matrix that is the inverse of 'x'
	xinv
}
