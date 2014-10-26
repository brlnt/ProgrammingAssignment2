## Put comments here that give an overall description of what your
## functions do


## function creating a special matrix (like a matrix object with set/get methods) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # set inverse null to start with
  inv <- NULL
  # set the matrix values and clear cacched inverse  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get matrix 
  get <- function() x
  
  # set the inverse 
  setinv <- function(inv_this) inv <<- inv_this
  
  # get the inverse 
  getinv <- function() inv
  
  # set/return function names as a list
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv
  )
  
}


## Function to calculate the inverse of a matrix created by the makeCacheMatrix function above
## It does get the cached inverse if stored first befora calculating and if inverse is not found
## calculate it and store for subsequent uses

cacheSolve <- function(x, ...) {
  # try to get the cached inverse first 
  inv <- x$getinv()
  # if found no need to calculate just return int 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise get the matrix data, calculate/store/return its inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


# to see it working in the following; after the first call to the cacheSolve
# subsequent calls print "getting cached data" instead of calculating the inverse


# my_matrix = rbind(c(1, 2), c(3, 4))
# matrix_obj =  makeCacheMatrix(my_matrix)
# cacheSolve(matrix_obj)
# cacheSolve(matrix_obj)
# cacheSolve(matrix_obj)

