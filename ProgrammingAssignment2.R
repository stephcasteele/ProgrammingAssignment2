# PROGRAMMING ASSIGNMENT 2

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initialize a variable
  set <- function(y) { #set values inside the function
    x <<- y 
    i <<- NULL
  }
  get <- function() x #gets x
  setinverse <- function(inverse) i <<- inverse #sets the inverse
  getinverse <- function() i #gets the inverse
  list(set = set, #saves it to the cache
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) { #checks if the inverse has already been calculated
    message("getting cached data")
    return(i)
  } #if not it solves the inverse of the matrix
  data <- x$get()
  i <- solve(data, ...) 
  x$setinverse(i)
  i
}