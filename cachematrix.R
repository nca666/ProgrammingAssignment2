## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	### setter for my matrix
	set <- function(m) {
		x <<- m
		inv <<- NULL
	}

	### getter for my matrix
	get <- function() {
		x
	}

	### setter for my inverse
	set_inv <- function(new_inv) {
		inv <<- new_inv
	}

	### gettter for my inverse
	get_inv <- function() {
		inv
	}

	### return a list with all my getters and setters
	list(set = set, get = get,
		set_inv = set_inv,
		get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	### check if the inverse is in the cache
	inv <- x$get_inv()

	### if it is in cache, show message and return it
	if (!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}

	### it isn't in cache, so calculate it
	mat <- x$get()
	inv <- solve(mat)

	### cache the result for future needs
	x$set_inv(inv)

	### return the result
	inv
}

