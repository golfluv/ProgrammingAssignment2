# My Solution
## cachematrix.R
## These are functions used to create an object that stores a matrix and caches its inverse.

## This function is used to create a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function (y) {
				x <<- y
				inv <<- NULL	
		}
		get <- function () x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set = set,
			 get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


## This function computes the inverse of the matrix that is created by the makeCacheMatrix that is shown above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse ()
        if (!is.null (inv)) {
        			message ("...getting cached data")
        			return (inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse (inv)
        inv
}
