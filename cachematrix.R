#This program creates two functions that are used to create a special object that 
#an invertible matrix and cache's its inverse. This program also introduces an 
# an operator <<- that can be used to assign a value to an object in an environment 
#that is different from current environment

## makeCacheMatrix is the first function that creates a special matrix, which is a
# list containing a function to:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
    x<<-y
    minv<<- NULL
  }
  get <- function() x
  setInverse <- function(inv) minv<<-inv
  getInverse <- function() minv
  list(get=get,set=set,setInverse=setInverse,getInverse=getInverse)
}


# The second function calculates the inverse of the invertible matrix created with 
#the above function. However, it first checks to see if the inverse has already been 
#calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the mean in the 
#cache via the setInverse function.



cacheSolve <- function(x, ...) {
  mInv <- x$getInverse()
  if(!is.null(mInv)){
    message("found cached")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data)
  x$setInverse(mInv)
  mInv
  
}



