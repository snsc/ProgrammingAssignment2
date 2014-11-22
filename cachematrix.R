## Programming Assignment 2
## Two functions that when use together allows for caching of the inverse of a matrix to save costly computation



# makeCacheMatrix - a function that creates a special "matrix" object and allows for caching of its inverse
# With 4 methods - set matrix, get matrix, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse
  i <- NULL 
  
  #set  -- assign x to the matrix, inverse set to null(unknown at this point)
  set <- function(matrix) {
    x <<-matrix
    inverse <<- NULL
  }
  
  #get  -- return the matrix, x
  get <- function() {
    x
  }
  
  #setInverse -- set inverse
  setInverse <- function(inverse) {
    i<<-inverse
  }
  
  #getInverse -- return the inverse, i
  getInverse <- function(){
    i
  }
  
  #list the methods available
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



# cacheSolve- a function that return the inverse of matix 
# 1. If the inverse has been computed prior, return from cache, 2. If not, compute the inverse and cache it
# Use in conjunction with makeCacheMatrix
# With 4 methods - set matrix, get matrix, set inverse and get inverse

cacheSolve <- function(x, ...) {
  
  #get the inverse 
  m <- x$getInverse()
  
  #if inverse already exist, return the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if not cached, then get the matrix and compute inverse
  data <- x$get()
  m <- solve(data)
  
  # once computed set the inverse value
  x$setInverse(m)
  m
  
  # return the inverse
}