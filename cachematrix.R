## The first function has four internal functions that 
## create a list which contains a matrix, an inverse of 
## the matrix and the capability to define and retrieve 
## the same matrix.  
## The second function takes the output of first 
## function as an input and checks if the inverse 
## matrix exists. If it does the second function 
## returns the inverse of the matrix, otherwise 
## it calculates the inverse matrix.

## This function will create the list with the input 
## matrix and store the inverse in the environment where
## the function is defined.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##assign the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##retrieve the assigned matrix
  get <- function() x
  
  ## set the inverse as an input from the user
  setinverse <- function(inverse) inv <<- inverse
  
  ## retrienve the assigned inverse
  getinverse <- function() inv
  
  ##return the SpecialMAtrix which is a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function will return the inverse of the 
## input special matrix which is returned from 
## makeCacheMatrix. The inverse value stored in
## the function environment will be fetched if 
## it exists. Otherwise the inverse will be caluclated.
########### This function assumes that the cached ###########
########### inverse is correct and is the inverse ###########
########### of the stored matrix.                 ###########

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##calculate inverse if the cached inverse 
  ##doesn't exist
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
