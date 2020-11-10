## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  
  ## part 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  
  ## Following the methodology used by the example in the example given in the course
  
  
  m <- NULL             ## We start my making M Null which hold our inverse value in the future
  set <- function(y) {  ## creating a new function called set
    x <<- y             ## <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. 
    m <<- NULL          ## anytime we get a new matrix the function makes 'M' Null
  }
  
  get <- function() x   ## get the value of the vector
  
  setinv <- function(inv) m <<- inv  ## sets the value of m in parent environment
  getinv <- function() m         ## gets the value of m 
  list(set = set, get = get, setinv = setinv, getinv = getinv)  ## Here we created the List with 4 components
  
  
}


## Write a short comment describing this function

##The following function calculates the inverse of the special "vector" created
##with the above function. However, it first checks to see if the inverse
##has already been calculated. If so, it gets the inverse from the cache
##and skips the computation. Otherwise, it calculates the inverse of the
##data and sets the value of the inverse in the cache via the setinv
##function.

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
