## These two functions calculate the inverse of a matrix and cache it, for when it is needed again.

## This function makeCacheMatrix creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	if (ncol(x)==nrow(x) && det(x)!=0){
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
			} 
			get <- function() x
			setinverse <- function(solve) inv <<- solve
			getinverse <- function() inv
			list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
			}
			else {
				message("The matrix x is not invertible")
			}
}


##cacheSolve calculates the inverse of a matrix introduced by the makeCacheMatrix function; if it has already been calculated before, returns the inverse of the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
