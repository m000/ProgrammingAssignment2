##  Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL              # cached inverse
	
	# mutator for the encapsulated matrix - also resets cached result to NULL
	set <- function(y) {
		# only reset inverse if matrix are not equal
		if ( !(all(dim(x) == dim(y)) && all(x==y)) ) { inv <<- NULL }

		# set encapsulated matrix in all cases
		x <<- y
	}
	
	# accessor - return encapsulated matrix
	get <- function() { x }
	
	# mutator for the inverse matrix
	set_inv <- function(inv_new) { inv <<- inv_new }
	
	# accessor for the inverse matrix
	get_inv <- function() { inv }
	
	# wrap everything into a list
	list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix().
## The inverse is computed only once. Cache results are used in subsequent calls.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	# return any non-NULL cached result
	inv <- x$get_inv()
	if(!is.null(inv)) { return(inv) }
	
	# no cached result exists - calculate it and return
	realmtx <- x$get()
	inv <- solve(realmtx, ...)
	x$set_inv(inv)
	return(inv)
}

## This function implements a loop for demonstrating the speed gains
## from caching the matrix inverse.
timeloop <- function(x, usecache=FALSE, n=1) {
	if (usecache) {
		message("Loop using cached inverse...")
		xc <- makeCacheMatrix(x)
		for(i in 1:n) {
			inv <- cacheSolve(xc)
		}
	}
	else {
		message("Loop without cached inverse...")
		xc <- x
		for(i in 1:n) {
			inv <- solve(xc)
		}
	}
	message("Done.")
	print(inv)
	message()
}

## Run the timing loop with caching disabled/enabled.
mtx <- matrix(c(3,1,2,1,1,1,2,1,1), nrow=3, ncol=3)
print(system.time(timeloop(mtx, usecache=FALSE, n=80000)))
print(system.time(timeloop(mtx, usecache=TRUE,  n=80000)))
