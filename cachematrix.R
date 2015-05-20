## Put comments here that give an overall description of what your
## functions do

# cacheSolve() gets the inverse of a matrix if it has already been calculated, if it hasn't it calculates it.
# makeCacheMatrix() is used to get and set the matrix and it's inverse and is used to define functions in cacheSolve()

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inv){
    inverse <<- inv
  }
  getinverse <- function(){
    inverse
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets
# the value of the inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  return(inverse)  ## Return a matrix that is the inverse of 'x'
}




# x = rbind(c(1, -1/4), c(-1/4, 1))
# m = makeCacheMatrix(x)
# m$get()
#
# #No cache in the first run
# cacheSolve(m)
#
# #Retrieving from the cache in the second run
# cacheSolve(m)





