## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(a = matrix()) {
  inverse <- NULL 
  set <- function(b) { 
    a <<- b 
    inverse <<- NULL
  }
  get <- function() a  
  setCacheMatrix <- function(solve) inverse <<- solve 
  getCacheMatrix <- function() inverse 
  list(set=set, get=get, setCacheMatrix=setCacheMatrix, 
       getCacheMatrix=getCacheMatrix)
  

}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by the function above.

cacheSolve <- function(x, ...) {
  inverse <- a$getCacheMatrix()
  if(!is.null(inverse)) {
    message("Getting cached inverted matrix")
    return(inverse)
  }
  inputmatrix <- a$get()
  inverse <- solve(inputmatrix, ...) 
  a$setCacheMatrix(inverse)
  inverse
}