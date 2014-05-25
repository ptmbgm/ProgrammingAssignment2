##The following code modifies the code given as part of Programming
##Assignment 2 of the R Programming Coursera offering.  The code 
##includes two functions which allow for cacheing the inverse of 
##a matrix.  The code requires the "Matrix" library.

library(Matrix)

##The first function generates a list which does four things: set a matrix,
##get a matrix, set an inverse of the given matrix, get the inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL  ##Initialize what will be the inverse matrix (local)

##Set the value of the initial matrix
##Note that the assignment treats x as global         

	  set <- function(y) {
                x <<- y
                I <<- NULL
        }

##get the initial matrix

        get <- function() x

##set the value of the inverse using the Matrix package (solve function)

        setinverse <- function(solve) I <<- solve
        
##get the value of the inverse 

	  getinverse <- function() I

##Now the list containing all four tasks

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cachesolve <- function(x, ...) {
        
##Assign the current value of inverse

	  I <- x$getinverse() 

##Check for cached inverse and return if non-null
       
	  if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }

##If no cached inverse exists, get the matrix and assign (local) to data 
         
	  data <- x$get()         

##Generate the inverse 

	  I <- solve(data, ...) 

##Set the inverse to I

	  x$setinverse(I) 

##Return the inverse
        
	  I 
}

##Create a test to make sure things run.  Try a diagonal matrix:

library(Matrix)

##The following generates a 10x10 matrix qith entries from 1 to 100

M<- matrix(1:100, 10,10)

##Extract the diagonal of the 10x10.  This will be invertible

d<- diag(M)

D<- Diagonal(10, d) 

##Construct the inverse of D and save it to check computation:

DI<- solve(D)

##Now run cachesolve and compare

M<- makeCacheMatrix(D)

cachesolve(M)

##Note that DI and cachesolve(M)coincide.


