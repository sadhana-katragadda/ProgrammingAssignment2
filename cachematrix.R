## Matrix inversion is a cpu intensive and time consuming operation for large matrices. 
##It would be good idea to cache the inversed matrices, so they can be retrieved easily.

# makeCacheMatrix creates a list of functions to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {
         x <<- y
        inv <<- NULL
      }
      get <- function() x
       setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv
       list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# CacheSolve function returns inverse of the matrix. 
#it first check to see if inverse value is available. 
#if is available, it returns the value.
#if not it computes the value and then returns the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}
