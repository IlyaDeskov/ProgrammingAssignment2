## Here are two functions that are used to create a special object
## that stores a square matrix and caches its inverse.


## This function checks dimensions of the input matrix and creates
## an object that stores created matrix and its inverse,
## if input matrix is square. Otherwise, it throws a message.
## Created object contains next four methods:
## "set" - sets matrix and clears its inverse
## "get" - returns current matrix value
## "getinv" - returns current value of inverse
## "setinv" - sets value of inverse

makeCacheMatrix <- function(x = matrix()) {
	if(ncol(x) == nrow(x)){
		inv <- NULL
		
		set <- function(y){
			x <<- y
			m <<- NULL
		}
		
		get <- function() x
		
		setinv <- function(i) inv <<- i
		
		getinv <- function() inv
	  
		list(set = set, get = get,
				setinv = setinv,
				getinv = getinv)
	}
	else{
		message("matrix is not square")
	}
}


## This function returns a matrix that is the inverse of input matrix.
## If inverse is already cached, function returns its value from cache.
## Otherwise, function calculates inverse via "solve(x)" function.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(x,...)
	x$setinv(inv)
	inv
}
