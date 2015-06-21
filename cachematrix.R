## This File contains the functions required to cache a matrix and its inverse
## for quick retrieval

## Function "makeCacheMatrix" caches a matrix and its inverse in memory.
## Arguments: Matrix
## Return: returns a list of functions for 
##                 a) getting the original matrix, 
##                 b) setting the inverse  
##                 c) retrieving the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	##initialize the invMatrix variable
	invMatrix <- NULL

	## function to set the inverse Matrix in cache
	setInv <- function(invX){
	invMatrix <<- invX
	}


	##function to retrieve the inverse matrix from cache
	getInv 	<- function() invMatrix

	##function to retrieve the original matrix from cache
	get	<- function() x
	
	##return the list of functions
	list(setInv = setInv, getInv= getInv, get = get)
}


## Function "cacheSolve" takes a matrix as an argument and returns its inverse
## from function cache, if available. Otherwise, it computes the inverse,
## stores it in cache and return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         invX <- x$getInv()
	 
	 ##check if the inverse is availe in cache
	 if (!is.null(invX)){
	 	message("retrieving cached data")
		return(invX)
	 }

	 ##calculate the inverse
	 invX <- solve(x$get())

	 ##store the inverse in memory
	 x$setInv(invX)

	 ##return the inverse matrix
	 invX

}
