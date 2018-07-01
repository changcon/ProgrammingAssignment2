## makeCacheMatrix makes an object of a list of 4 functions set, get, setinv, and getinv 
## associated with your passed matrix x
## set() sets your matrix and nulls your inverse matrix i
## get() outputs your set or passed in matrix x
## setinv() sets your inverse matrix
## getinv() outputs your inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        i <<- NULL
	set <- function(y) {
        x <<- y
        i <<- NULL
        }
	get <-function() x
	setinv <- function(invM) { i <<- invM }
	getinv <- function() i
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## cacheSolve takes as input the object created in the function makeCachMatrix
## if the inverse matrix i is null then it calculated the inverse with 
## function solve(). 
## if the inverse matrix i is not null then it outputs 
## a message that indicates it's retrieving a cached 
## inverse matrix i that was previously set or calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinv(i)
	i
}

## Sample output - putting in an intial matrix n1, then setting using matrix n2

## > n1 <- matrix(c(6, 2, 8, 4), nrow = 2, ncol = 2)
## > testMatrix<-makeCacheMatrix(n1)
## > cacheSolve(testMatrix)
##       [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75

## > n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## > testMatrix$set(n2)
## > cacheSolve(testMatrix)
##      [,1] [,2]
## [1,]    3    7
## [2,]    1    5
## > cacheSolve(testMatrix)
## getting cached data
##      [,1] [,2]
## [1,]    3    7
## [2,]    1    5


