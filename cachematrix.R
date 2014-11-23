## A set of two functions, one that cache the inverse of a matrix and another 
## one that solves the inverse of a matrix and retrieves the cached inverse 
## if the inverse has already been calculated.

## This function creates a matrix object that can cache the inverse of a 
## given matrix.
makeCacheMatrix <- function( x = matrix() ) {
    
	## Initialization of the inverse matrix.
	m <- NULL 
	
	## Function that assign makeCacheMatrix the input variable.
    set <- function( matrix ) {
            x <<- matrix
            m <<- NULL
    }
	
	## Function that returns x.
    get <- function() x

	## Function that assign the inverse of the matrix to m.
    setInverse <- function(inverse) {
        m <<- inverse
    }

	## Function that returns the matrix m.
    getInverse <- function() m

	## makeCacheMatrix function returns a list of four elements, each 
	##element is a function defined earlier.
    list(set = set, 
		 get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function solves the inverse matrix for the one returned 
## by the previous function makeCacheMatrix. If it has already 
## been solved and the matrix does not changed it will retrieve 
## the cached inverse
cacheSolve <- function(x, ...) {

	## Assign the inverse matrix of x to m
    m <- x$getInverse()

	## Checking if it has been already solved an inverse for x
    if( !is.null(m) ) {
            message("Retrieving cached inverse")
            return(m)
    }

	## Reads the matrix 
    df <- x$get()
	
	## Solves the inverse for the matrix
    m <- solve(df) %*% df
	
	## Sets the inverse matrix for x
    x$setInverse(m)
	
	## Return the inverse matrix for x
    m
}

## Example for usig the above functions.

##Generating a 5 by 5 random matrix.
set.seed(12345)
matrix <- stats::rnorm(25)
dim(matrix) <- c(5,5)
matrix

## Cache the generated matrix.
cachedMatrix = makeCacheMatrix(matrix)

## Call the cacheSolve function twice to show that first it
## solves the inverse and second time it returns the cached 
## inverse matrix.
cacheSolve(cachedMatrix)
cacheSolve(cachedMatrix)
