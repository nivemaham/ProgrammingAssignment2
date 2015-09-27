# this function creates the set, get, setInverse, getInverse functions 
makeCacheMatrix <- function(x = matrix()) {
  #create and initialize var inv to NULL
  inv <- NULL
  
  # sets the given value to x and NULL to inv
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  
  #returns x
  get <- function() x
  
  # sets given value to inv
  setInverse <- function(mean) inv <<- mean
  
  # returns inv
  getInverse <- function() inv
  
  # lists all the functions created 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# returns the inverted matrix value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # gets inverse matrix value of x
  invers <- x$getInverse()
  
  # if the inverse is not null, returns the data from cache
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  
  # if the inverse value is null, we calculate it. 
  # retrive data to calculate inverted martix
  data <- x$get()
  
  # calculate inverted matrix value
  invMat <- solve(data, ...)
  
  # set the value. so it stores in cache
  x$setInverse(invMat)
  
  #return calculated value
  invMat
}
