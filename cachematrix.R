## Functions to satisfy conditions of R programming course rprog-012
## assignment #2

## creates an "object" to store a matrix and its inverse for later use 
makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL
     set<-function(y){
          x<<-y
          inv<<-NULL
     }
     get<-function() x
     setInv<-function(inverted) inv<<-inverted
     getInv<-function() inv
     list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve solves for the inverse if necessary and caches the value, 
##otherwise reads and returns cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     ##safety - if non list is passed in (makeCacheMatrix not called)
     ##then call it.
     repx<-x
     if (!is.list(x)){
          repx<-makeCacheMatrix(x)
     }
     x<-repx
     
     inv <- x$getInv()
     if(!is.null(inv)){
          message ("getting cached inverse")
     } else {
          message("solving and caching inverse")
          mat <- x$get()
          inv <- solve(mat)
          x$setInv(inv)
     }
     inv
}
