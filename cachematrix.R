## These two functions provide a way to obtain the inverse of a nonsingular matrix
# where the inverse is cached (stored) for further computation.
# On repeated calls to cacheSolve(), the matrix inverse is not recalculated 
# unless the matrix changes.
##
# Usage:
# matrix.data <-matrix(c(1,1,1,4),nrow=2,ncol=2) # Create matrix 
# data <-makeCacheMatrix(matrix.data)
# data$get() ## view matrix
# data$getinv()  ## NULL object because inverse has not been computed
# cacheSolve(data) ## Obtain matrix inverse
# data$getinv()  ## view matrix inverse
# cacheSolve(data)  ## cached value is used for inverse
# matrix.data <-matrix(rnorm(9),nrow=3,ncol=3) # create new matrix
# data <-makeCacheMatrix(matrix.data)
# cacheSolve(data)
# data$getinv()

## makeCacheMatrix returns a list containing four functions:
# set(), get(), setinv(),and getinv()

makeCacheMatrix <-function(x=matrix()){
  # Initialize matrix inverse as NULL object
  xinv <- NULL;
  
  # Set the values of the matrix
  # Initialize matrix inverse as NULL object
  
  set <- function(y){
    x <<-y;
    xinv <<- NULL;
  }
  # Get the matrix
  get <-function() 
    x
  # Set the matrix inverse
  setinv <- function(matinv) 
    xinv <<- matinv
  
  # Get the stored matrix inverse
  getinv <-function() 
    xinv
  
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}



## cacheSolve returns the inverse of a matrix.
# cacheSolve accepts the output of makeCacheMatrix() as an input
# and determines whether the inverse matrix is already stored 
# before computing the inverse.
# 

cacheSolve <- function(x,...){
  xinv <-x$getinv();
  if(!is.null(xinv)){
    message("Getting cached matrix inverse.")
    return(xinv)
  }
 matrix.data <-x$get();
 xinv <-solve(matrix.data);
 x$setinv(xinv);
 xinv
}




