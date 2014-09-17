## Returns an object representing a matrix and its inverse
## functions:
##    $set(matrix()) - caches the given matrix
##    $get() - returns the matrix
##    $setinverse(matrix()) - caches the matrix inverse
##    $getinverse() - returns the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL             # holds the inverse
	set <- function(y) {
		x <<- y         # holds matrix to be inverted
		m <<- NULL      # initialize the inverse
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse  # sets inverse in env of our cacheMatrix obj 
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x = matrix(), ...) {
	# if the cacheMatrix obj holds an inverse, return it
	# otherwise compute it
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	inverse <- solve(matrix, ...) # calculate the inverse
	x$setinverse(inverse)
	inverse
}
