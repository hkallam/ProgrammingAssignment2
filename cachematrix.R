## makeCacheMatrix  takes square matrix as an argument.
## sets the inverse of a matrix by invoking the setinverse from   cacheSolve
## returns the list containg the subfunctions like 1.set 2.get 3.setinverse 4.getinverse
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL         
  set<-function(n)
  {  
    x<<-n;     ## superassaingnment operator is used to asaign a value to the variable in the parent function(the one step above )
    i<<-NULL;
  }
  get<-function()
    x
  setinverse<-function(inverse)
  {
    i<<-inverse
  }
  getinverse<-function()
    i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##cacheSolve gets the inverse matrix(i.e already calculated matrix) by calling getinverse,checks if calculated or not before
##otherwise takes the given matrix ,calculates the inverse matrix by solve() and returns it.
cacheSolve <- function(x, ...) {
  i=x$getinverse()
  if(!is.null(i))
  {
    message("got cached data")
    return(i)
  }
  data=x$get()
  inv=solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
