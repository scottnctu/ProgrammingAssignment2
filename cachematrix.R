# Course: R Programming
# Programming Assignment 2: Lexical Scoping
# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
#   (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {  	# input x will be a matrix
  iv <- NULL
  
  set <- function(y) {		# takes an input matrix
    x <<- y					# saves the input matrix
    iv <<- NULL				# resets the mean to NULL, basically what happens when a new object is generated.
  }
  
  get <- function() x		# this function returns the value of the original matrix
  
  setinverse <- function(inverse) iv <<- inverse
  # this is called by cacheSolve() during the first cacheSolve() access and it will store the value using superassignment
  
  getinverse <- function() iv
  # this will return the cached value to cacheSolve() on subsequent accesses
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


cacheSolve <- function(x, ...) {		# the input x is an object created by makeCacheMatrix
  ## Return a matrix that is the inverse of 'x'
  
  iv <- x$getinverse()		# accesses the object 'x' and gets the value of the inverse
  
  if(!is.null(iv)) {		# if inverse was already cached (not NULL) ...
    message("getting cached data")		# ... send this message to the console
    return(iv)							# ... and return the inverse ... "return" ends
  }
  
  data <- x$get()						# we reach this code only if x$getinverse() returned NULL
  iv <- solve(data, ...)				# if iv was NULL then we have to calculate the inverse
  x$setinverse(iv)						# store the calculated inverse value in x (see setinverse() in makeCacheMatrix
  iv									# return the inverse to the code that called this function	
  
}
