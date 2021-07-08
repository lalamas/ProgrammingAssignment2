## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ## declaration and initialization variable  inver to NULL
      inver <- NULL
      
      ## declaration method to set the matrix
      set <- function(y){
              x <<- y
              inver <<- NULL
      }
      
      ## declaration method to get the matrix
      get <- function(){
              x
      }
      
      ## Declaracion method to set inverse of the matrix
      setInver <- function(solveMatrix){
              inver <<- solveMatrix
      }
      
      ## Declaracion method to get inverse of the matrix
      getInve <- function(){
              inver
      }
      
      #@ Return list with definition methods
      list(set = set, get = get, 
           setInver = setInver, 
           getInver = getInver)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix is the inver to x
    mat <- x$getInver()
  
    ## if matrix is not null, used data the cache
    ## and exit function, return the matrix
    if( !is.null(mat) ) {
        message("cached data")
        return(mat)
    }
  
    ## Get the matrix 
    data <- x$get()
  
    ## Calculate the inverse 
    mat <- solve(data) %*% data
  
    ## Set the inverse
    x$setInver(mat)
  
    ## Return matrix
    mat
}
