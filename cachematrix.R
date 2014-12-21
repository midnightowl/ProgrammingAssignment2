## Given a square matrix, makeCacheMatrix will cache the matrix in x and return
## a list of functions for that matrix.  cacheSolve will take the list created by
## makeCacheMatrix and either recall the inverse from the cached value or 
## calculate the inverse and store it in cache.

## Given a square matrix, makeCacheMatrix will cache the matrix in x and return
## a list of functions for that matrix.

makeCacheMatrix <- function(x = matrix()) {

	v <- NULL

	set <- function(y){
		x <<- y
		v <<- NULL
	}

	get <- function() x

	setinv <- function(inverse) v <<- inverse

	getinv <- function() v

	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)

}



## cacheSolve will take the list created by
## makeCacheMatrix and either recall the inverse from the cached value or 
## calculate the inverse and store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	v <- x$getinv()

	if(!is.null(v)){

		message("getting cached data")

		return(v)

	}

	matrix <- x$get()

	v <- solve(matrix,...)

	x$setinv(v)

	v


}
