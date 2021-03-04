
# This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                       #inv is set to NULL by default
  set <- function(y){                               #value of inv is set inside of function set 
    x <<- y                                  
    inv <<- NULL
  }
  get <- function() x                               # get the matrix value
  setinverse <- function(inverse) inv <<- inverse   # set the inverse of the matrix value 
  getinverse <- function() inv                      # get the inverse of the matrix value   
  list(get = get, set = set, 
       getinverse = getinverse,
       setinverse = setinverse )                    # print a list of the above functions to the console
}


# This function computes the inverse of the special matrix retunred by 
# by makeCacheMatrix. If the inverse has already been calculated then
# cacheSolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                            # if the inverse of the matrix argued has been cached, then it is retrieved
  if(!is.null(inv)) {                              # if the value of the inversed matric is not NULL then it is returned to the console
    message("getting cached inversed matrix")    
    return(inv)
  }
  data <- x$get()                                  # when the matrix inverse is not cached the matrix argued is set
  inv <- solve(data, ...)                          # the inverse is calculated
  x$setinverse(inv)                                # the inverse is set
  inv                                              # the inverse is printed to the console 
}
