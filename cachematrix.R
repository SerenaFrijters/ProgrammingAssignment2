## This code can cache the reverse of a matrix, so it only has to be computed once.

## The function makeCacheMatrix stores an object with 4 functions to set and get both a matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  } ## set default value for m (NULL) and set x to input value
  get<-function() x #
  setinv <-function (solve) m <<-solve #compute inverse of matrix
  
  getinv<-function() m #get inversed matrix
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## cacheSolve returns the cached value of the inverse matrix if it exists. Else it computes the inverse.

cacheSolve <- function(x, ...) {

  m<-x$getinv() #check for cached inverse of matrix
  if(!is.null(m)){
    message("getting chached data")
    return(m) #returned cached inverse
    
    
  }
  data=x$get()
  m<-solve(data) #compute inverse matrix
  x$setinv(m) #set inversed matrix so it can be retrieved.
  m
  
     
}
