## Creates a matrix object capable of caching an inverse and returning it. Also creates the function to calculate the inverse


library('MASS')

## creates the matrix object capable of caching an inverse
makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse<- function () i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

 ##solves the inverse of a matrix and stores it in the makeCacheMatrix object

cacheSolve <- function(x,...){

  i <- x$getinverse()
  if(!isnull(i)){ 
    ## if the matrix's solution is not empty, returns the matrix's solution 
    mesage("getting cached data")
    return(i)
  }
  data <-x$get()
  if(nrow(data) ==2 && ncol(data)== 2){
    xinverse = solve(data) ## if the matrix is a 2x2 matrix it uses solve
  }
  else{ xinverse = ginv(data)} 
  ## if the matrix is not a 2x2 matrix it will use the built in ginv function
  x$setinverse(xinverse) ##stores the inverse matrix in the x object
  xinverse ##returns the inverse matrix 
}
