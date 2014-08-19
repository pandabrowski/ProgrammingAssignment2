##  This function creates a list of functions that store matrix inversion in cache. function list is returned,
## contains both setters and getters for matrix and inverted matrix.

## first function is used to as a storage object, second function is used for inversion calculation and access

## usage example
# x <- matrix(1:4,2,2)                      // Create a matrix 
# cx <- makeCacheMatrix(x)                  // Create cached matrix object
# cacheSolve(cx)                            // Return inverted matrix
# cacheSolve(cx)                            // Return inverted matrix (fetch from cachce)






## Function defines internal variables for storage of matrix+inverted matrix and 4 accessor functions  for them.
# input - matrix
# output - list of 4 functions

makeCacheMatrix <- function(x = matrix()) {

  
    internal_var_matrix_inverse <- NULL
    #setter function definition
    set <- function(set_matrix) {
                                x  <<- set__matrix
                                internal_var_matrix_inverse <<- NULL
                               }
    #getter function definition
    get <- function()          {
                                x
                                }
    #inverted matrix setter
    set_inverse <- function(input_im) internal_var_matrix_inverse <<- input_im
    #inverted matrix getter
    get_inverse <- function() internal_var_matrix_inverse
    #return value - list of functions
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
  }
  
  



## Function calculates inversion of matrix if it is not cached otherwise use precalculated values.
## requires list of functions returned from makeCacheMatrix function as an input

#input CacheMatrix function list 
#output matrix inversion
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
  
  
  
}
