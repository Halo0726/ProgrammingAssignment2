## compute the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	cacheMatrix <- NULL
	setMatrix <- function(y) {
	x <<- y
	cacheMatrix <<- NULL
	}
	getMatrix <- function() x
	setCache <- function(inverse) cacheMatrix <<- inverse
	getCache <- function() cacheMatrix

	list(setMatrix = setMatrix,
	getMatrix = getMatrix,
	setCache = setCache,
	getCache = getCache)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	cacheMatrix <- x$getCache()
	if (!is.null(cacheMatrix)) {
		message("getting cached matrix")
		return(cacheMatrix)
	}

	dMatrix <- x$getMatrix()
	cacheMatrix <- solve(dMatrix, ...)
	x$setCache(cacheMatrix)
	cacheMatrix 
}
