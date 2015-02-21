
## Two functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## This is a function of x, in which x is a matrix
	m <- NULL
	set <- function(y) {
		x <<- y     ## Using <<- we assign a value to an object in a different environment
		m <<- NULL
	}
	
	get <- function() x
	setinverse <- function(solve) m <<- solve ## To inverse the matrix
	getinverse <- function() m
## Get returned a list that contains the above functions, so we can call them afterwards
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the function above. 
## If the inverse has already been calculated, and the matrix has not changed, then this function 
##  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {    ## If the matrix is the same and the inverse has been calculated, then write the message
		message("getting cached data")
		return(m)
	}
	
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m ## Retrieve the inverse
}
