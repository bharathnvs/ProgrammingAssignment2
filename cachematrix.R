## Put comments here that give an overall description of what your
## functions do

## This function "makeCacheMatrix" rerurns alist to set the matrix, 
## get the matrix ,set the inverse, and get the inverse

### The list obtained from the first functoion will be sed as input in the second function.

makeCacheMatrix <- function(x = matrix()) {
              
   INV =NULL
   set = function(y){
        
         x<<-y
         INV<<-NULL
}
   
   get = function()  x
   
   setINV = function(inverse) INV <<- inverse
   
   getINV = function()  INV

   list (set=set, get= get, setINV = setINV , getINV = getINV)
}


## Thse second function tries to find whether the present matrix's inverse was already caluclated or not. If 
## it is already calculated the value will be collecetd from the Cache or otherwise the following function will calculate the inverse

cacheSolve <- function(x, ...) {
                             ## Return a matrix that is the inverse of 'x'
  
  INV = x$getinv()
   
  if(!is.null(INV))         ##Checking the given matrix is same or not, if yes, the value will be taken from Cache
   {
     message("getting cached data")
     return(INV)
   }
                            ## Or otherwise the inverse value will be calculated
  
  mat.data = x$get()
  INV = solve(mat.data, ...)
  
  x$setINV(INV)
  return(INV)
}
