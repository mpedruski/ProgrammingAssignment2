## makeCacheMatrix and cacheSolve can be used to calculate and store the inverse of a matrix, preventing unnecessary re-calculation of the matrix inverse. Modelled after the makeVector and cachemean example functions for programming assignment 2. 


## makeCacheMatrix defines the object inv and three functions and stores the functions' output in a list. Functions include get (which returns the matrix that is supplied as an argument to makeCacheMatrix), setinverse (which sets inv to be equal to its argument y), and getinverse (which returns inv).

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	get <- function() x
	setinverse <- function(y) inv <<- y
	getinverse <- function() inv
	list(get = get, setinverse=setinverse, getinverse=getinverse) 
}


## cacheSolve takes as its argument mcm an object returned by a call to makeCacheMatrix, evaluates whether the matrix passed to CacheMatrix already has its inverse solved or not, and returns the inverse matrix, either by solving it or drawing it from cache if it has already been solved. 

cacheSolve <- function(mcm, ...) {
	inv <- mcm$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- mcm$get()
	inv <- solve(data)
	mcm$setinverse(inv)
	inv
        ## Returns a matrix that is the inverse of 'x'
}
