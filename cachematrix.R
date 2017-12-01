## This script contains the following functions.
# makeCacheMatrix(<invertible matrix>): 
# This function receives as input a matrix object.
# It then creates a composite object which has two attributes.
# Its first attribute is a matrix object to which it
# assigns the input matrix. Its second attribute is the
# inverse of the input matrix which gets initialized with
# the NULL value upon the creation of the composite object.
# The created composite object also has member functions.
# set() initializes the matrix object and inverse matrix object
# attributes of the composite object. get() returns the matrix
# object of the composite object. setsolve() sets the inverse
# matrix attribute of the composite object into an input matrix.
# getsolve() returns the inverse matrix attribute of the composite
# object. The makeCacheMatrix function returns a list of its member
# functions which are accessible via the $ operator.
#
# cacheSolve(<composite object output of makeCacheMatrix function>):
# This function receives as input a composite object created using
# the makeCacheMatrix function. This function returns the value of
# the inverse matrix attribute of the composite matrix if is not
# NULL. It computes for the inverse matrix attribute of the composite
# object if the object's inverse matrix attribute is NULL and saves its
# value which is accessible using the getsolve function.

# Note: The names of the member functions of makeCacheMatrix have been revised
# to make them more intuitive for usage. The signature of the makeCacheMatrix
# function has been kept the same though instructed.

## Write a short comment describing this function
# x - a matrix; assumed to be invertible
# s - the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse of the input matrix data into NULL
  s <- NULL
  
  # initialize the matrix data object
  instantiate <- function(y){
    # initialize the matrix data into the value of the input matrix y
    x <<- y
    
    # initialize the inverse of the matrix data into NULL
    s <<- NULL
  }
  
  # get the matrix data
  getMatrix <- function() x
  
  # set the inverse of the matrix data
  setInverseMatrix <- function(sol) s <<- sol
  
  # get the inverse of the input matrix data
  getInverseMatrix <- function() s
  
  # return a list of the member functions accessible using $ operator
  return(list(instantiate = instantiate, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix))
}


## Write a short comment describing this function
# x - a matrix; assumed to be invertible
# s - the inverse of the input matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getInverseMatrix()
  
  # check if the returned inverse of the input matrix x is NULL or not
  # if the returned matrix inverse is not null, return it
  if(!is.null(s)){
    message("getting the inverse of matrix")
  } else { # else if the returned inverse matrix is NULL, compute for it and return it
    # get the matrix data of the input x 
    data <- x$getMatrix()
    
    # compute for the inverse of the matrix data using solve()
    s <- solve(data)
    
    # set the inverse matrix attribute of the input x to the computed inverse matrix
    x$setInverseMatrix(s)
  }
  
  # return the retrieved/computed inverse matrix
  return(s)
}
