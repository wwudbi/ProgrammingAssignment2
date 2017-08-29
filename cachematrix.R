## Put comments here that give an overall description of what your
## functions do
## This function will cache potentially time-consuming computations. 
## For example, taking the mean of a numeric vector is typically a fast operation. However, 
## for a very long vector, it may take too long to compute the mean, 
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a vector are not changing, 
## it may make sense to cache the value of the mean so that when we need it again, 
## it can be looked up in the cache rather than recomputed

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y) {
       x <<- y    
       m <<- NULL  
  }  
  get <- function() x  
  setsolve <- function(solve) m <<- solve  
  getsolve <- function() m  
  list(set = set, get = get,       
       setsolve = setsolve,       
       getsolve = getsolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
   m <- x$getsolve()
   if(!is.null(m)) {
       message("getting cached data")
       return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
   ## Return a matrix that is the inverse of 'x'
}
