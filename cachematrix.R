
makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  inv = NULL
  set = function(y) {
    ## use `<<-` to assign a value to an object in an environment 
    ## different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
##list of functions set/get matrix, set/get inverse of matrix
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  ## check if the inverse has already been calculated
  if (!is.null(inv)){
    ## if yes, then get it from the cache and skip the computation
    message("getting cached data")
    return(inv)
  }
  ## otherwise, calculate the inverse 
  data = x$get()
  inv = solve(data, ...)
  ## set the value of the inverse in the cache via the setinv function
  x$setinv(inv)
  return(inv)
}

##
#simple test of above function
# a <- makeCacheMatrix()
# a$set(matrix(1:4, 2, 2))
# a$get()
# cacheSolve(a)     
# cacheSolve(a)    #second time cached inv data should be outputted -unless matrix changed. 

# a$set <-makeCacheMatrix()
# a$set(matrix(2:8, 2, 2))