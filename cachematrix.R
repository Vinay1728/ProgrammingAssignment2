## The below functions are used to find the inverse of a matrix using 
## caching or memoization.


## This function creates an matrix object to cache the inverse

makeCacheMatrix <- function(matrix_var = matrix()) {
      inverse <- NULL
      
      set <- function(x) {
        matrix_var <<- x;
        inverse <<- NULL;
      }
      
      get <- function() {
        return(matrix_var);
      }
      
      setinverse <- function(inv) {
        inverse <<- inv;
      }
      
      getinverse <- function() {
        return(inverse);
      }
      
      return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}




## This function computes the inverse of the matrix object
## returned by the "makeCacheMatrix" function above.


## If the contents of the matrix have been unchanged and inverse of it
## has to be calculated, then "cacheSolve" retrieves it from the cache.

cacheSolve <- function(matrix_var, ...) {
  # Checking if inverse of the matrix has already been calculated
  inverse <- matrix_var$getinverse()
  # If the inverse has been calculated previously
  if(!is.null(inverse)) {
    # Showing an message to the user in the console about it
    message("Getting the inverse from the cache")
    # Returning the inverse
    return(inverse)
  }
  
  # Getting the matrix
  data <- matrix_var$get()
  # Calculating the inverse
  inverse <- solve(data, ...)
  # Storing the inverse in the cache
  matrix_var$setinv(inverse)
  # Returning the cache
  return(inverse)
}


## Example
## m <- matrix(c(1,2,2,2,1,2,2,2,1), 3, 3)
## matrix_object <- makeCacheMatrix(m)
## cacheSolve(matrix_object)

## OUTPUT:
## [,1] [,2] [,3]
## [1,] -0.6  0.4  0.4
## [2,]  0.4 -0.6  0.4
## [3,]  0.4  0.4 -0.6