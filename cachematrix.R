## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  makeCacheMatrix<-function(x=matrix()){
    inv<-NULL
    
    ## define the set funcation
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    
    ## define the get function
    get<-function()x
    
    ## define the setInverse function
    setInverse<-function(inverse)inv<<-inverse
    
    ## define the getInverse function
    getInverse<-function()inv
    
    ## return the list
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
  
      ## check whether the inverse has been existed
      if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## if not, calculate the inverse
    mat <- x$get()
    
    ## calculate the inverse by solve
    inv <- solve(mat, ...)
  
    ## set the inverse
    x$setInverse(inv)
    return(inv)
}
