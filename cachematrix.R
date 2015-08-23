## Create the function makecachematrix
## this function creates a list

makeCacheMatrix <- function(x = matrix()) {
xinv<- NULL ## we'll store the result of the inversion here
set<- function(y){
  x<<- y
  xinv<<- NULL

}
get<- function()
  setInv<- function(inv)
    xinv<<- inv ## set the inversed matrix
getInv<- function()
  xinv # returns the inversed matrix

list(set = set,get = get,
     setInv = setInv,
     getInv = getInv)

}


## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getInv() #get the inverse matrix from x
  if(!is.null(m)){ #if we already have the inversion
    message("getting data cached")
    return(m) #return the inversion
  }
  data <-x$get()
  m<- solve(data) #solve it
  x$setInv(m) #set it
  m
}

