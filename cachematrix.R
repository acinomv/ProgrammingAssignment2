#creates a special "matrix"  that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    #sets value
    x <<- y
    #clears cache
    m <<- NULL 
  }
  #function to get matrix value
  get <- function() x
  #sets inverse
  setinverse <- function(inverse) m <<- inverse
  #gets inverse
  getinverse <- function() m
  #returns list with set, get, setinverse, and getinverse
  list(set = set,  get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

#retrieves inverse from cache if already calculated
#calculates inverse from above if cache is empty
cacheSolve <- function(x, ...) {
  #gets cached inverse
  m <- x$getinverse()
  #returns what's in cache if there's a value present
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #gets, calculates, caches, and returns if cache is empty
  #retrieves matrix value
  data <- x$get()
  #calculates inverse
  m <- solve(data, ...)
  #caches result
  x$setinverse(m)
  #returns cached inverse
  m
}
