## The following functions provide a mechanism to create and
## use a matrix which can cache its inverse.

## 'makeCacheMatrix' returns a matrix, which can cache
## its inverse, as used by 'cacheSolve'.

makeCacheMatrix <- function(m = matrix()) {
	# Set the inverse matrix to NULL.
	inv <- NULL

	# 'set' sets the matrix.
	set <- function(y) {
		m <<- y
		inv <<- NULL
	}

	# 'get' returns the matrix.
	get <- function() m

	# 'setInverse' caches the inverse matrix.
	setInverse <- function(solve) inv <<- solve

	# 'getInverse' returns the cached inverse matrix.
	getInverse <- function() inv

	# Return a list containing the functions
	# 'set', 'get', 'setInverse' and 'getInverse'.
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## 'cacheSolve' computes the inverse of a matrix. If the inverse
## has already been calculated, 'cacheSolve' returns the cached
## inverse.

cacheSolve <- function(x, ...) {
	# Get the cached inverse matrix.
	inv <- x$getInverse()

	# If the inverse matrix is cached (not NULL),
	# the cached inverse matrix is returned.
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	# If the inverse matrix is not cached, load the matrix ...
	m <- x$get()
	# ... compute the inverse matrix, using 'solve' ...
	inv <- solve(m, ...)
	# ... cache the computed value ...
	x$setInverse(inv)
	# ... and finally, return the inverse matrix.
	inv
}
