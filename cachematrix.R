
## makeCacheMatric function is similar to a notion of class in OOP: 
## it allows caching the inverse of the matrix for future usa

## makeCacheMatix returns a list of four functions to work with a matrix passed as an argument

## set(m), get() - initializing the matrix, and getting the matrix back
## setinverse(m) - calls solve(m) for the matrix, and assings it to the "cache" variable
## getinverse() - return "cache" variable: NULL, or the inverse matrix if setinverse(m) was invoked before

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	
	## set a new matrix to be used
	## when new matrix is set, inverse result should be reset to NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## return the matrix
	get <- function() x
	
	## setinverse(m) - invokes solve(m), and puts the result into inv variable
	setinverse <- function(solve) inv <<- solve
	
	## getinverse() - returns a NULL, or a result of solve(m) if setinverse(m) was invoked before
	getinverse <- function() inv
	
	## construct the list of functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve is a function that should be used to get an inverse for a given matrix from cache

## Sequence of operations to use cached inverse matrix:
## 1. call m_encapluslated <- makeCacheMatrix(matrix m): you get functions to work with the matrix m
## 2. Each time you need to use an inverse matrix call m_inverse <- cacheSolve(m_encapsulated)  

cacheSolve <- function(x, ...) {
        ## Check if inverse matrix is already cached 
	m <- x$getinverse()

	if(!is.null(m)) {
		## return cached data if it is not null
		return(m)
	}
	
	## calculate inverse matrix if this was not done before
	data <- x$get()
	m <- solve(data, ...)
	
	## set inverse matrix using setinverse function of makeCacheMatrix() for future use
	x$setinverse(m)
	m
}
