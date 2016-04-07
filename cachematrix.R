## The two functions allow the inverse matrix to cashed and retrieved when required.


## The makeCacheMatrix() fucntion creates a list that stores the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  
  #Define Set function
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  ##Define get function
  get = function() x
  
  ##Define Set inverse function
  setinv = function(inverse) inv <<- inverse 
  
  ## Define GEt inverse function
  getinv = function() inv
  
  ##Set list names
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  
}


## The cacheSolve function returns the inverse of the matrix.
## The inverse matrix is only calculated if the value hasn't be cached earlier

cacheSolve <- function(x, ...) {
  
  ## Retrieve the cached inverse matrix
  inv = x$getinv()
  
  ## If cache is null, calculate the inverse
  if (is.null(inv)){
    
    matrix = x$get()
    inv = solve(matrix, ...)
    
    ##Cache the inverse matrix
    x$setinv(inv)
  }
  
  ##Return Inverse Matrix
  inv
}
