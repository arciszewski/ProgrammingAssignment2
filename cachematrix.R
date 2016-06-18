## Here we're trying to write a wrapper object for square matrix, that will
## contain computed inversed matrix. Inversed matrix will be computed on-demand,
## computation should happen only once.

## Wrap matrix given with object, which may contain reversed matrix alongside

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolved <- function(solved) s <<- solved
	getSolved <- function() s
	list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}


## Solves given wrapped matrix, i.e. calculates it's reversed matrix (if it wasn't already been calculated)

cacheSolve <- function(x, ...) {
	s <- x$getSolved()
	if(!is.null(s)) {
		message("Getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data)
	x$setSolved(s)
	s
}
