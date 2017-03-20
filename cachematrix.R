## This pair of functions enable caching the inverse of a matrix

## The first step is to create the matrix that can cache its inverse
## essentially, creates an object with matrix and its inverse as data structures
## and operators to set the value and inverse, and return them

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y) {
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setinv<-function(solve) inv<<-solve
	getinv<-function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function gets the inverse if already created
## otherwise calculates the inverse and sets it in the object

cacheSolve <- function(x, ...) {
      inv<-x$getinv()
	if(!is.null(inv)) {
		message("getting inverse from cached value")
	      return(inv)
	}
	mat<-x$get()
	inv<-solve(mat)
        x$setinv(inv)
	inv
}
                
## tried to follow example closely. But not sure line 14 is right, or if so, why we can't use the setinv operator instead of inv<solve(mat) in line 30.
