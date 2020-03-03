## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## makeCacheMatrix - function which creates cache matrixes to find the inverse of
## make i start at null and go back to null every time that y is set
## set; set the matrix, rather than creating a new variable/re-doing the function
## get; get the matrix
##setinverse ; set the value for i
##getienverse; retrieve i
##cacheSolve; function which uses solve to find the inverse of the cached matrix
##i is the variable that holds the inverse matrix, but goes back to null everytime the matrix is changed
##makeCacheMatrix$getinverse() brings up the cached inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve creates a function which finds the inverse of the matrix made by makeCacheMatrix which is then stored in 
##a cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

