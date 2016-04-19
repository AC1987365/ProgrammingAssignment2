### the makeCacheMatrix function gets the x matrix, calculate the inverse
### and stores it in the CacheInverse_x object. The x matrix is cached for
### the cacheSolveMatrix as well.

makeCacheMatrix<- function(x=matrix()){
  
  x <<-x
  
  CacheInverse_x <- NULL
  CacheInverse_x  <<- solve(x)
  
}

########################################################################
### the cacheSolveMatrix calculates the inverse of the object x (even if 
### the one from makeCacheMatrix). The if-statement checks, if all four
### values of the inverse matrix from x is equal to the inverse calculated
### in the makeCacheMatrix. Depending on whether it is true or false, the
### cached or the new calculated inverse matrix is returned.

cacheSolveMatrix <- function(x){
  
  Inverse_x <- solve(x) 
  
  if(sum(CacheInverse_x==Inverse_x)==4){
    message("inverse matrix from cache")
    return(CacheInverse_x)    
  }else{
    
    message("inverse matrix is calculated new")
    return(Inverse_x)
  }}

### Testing the makeCacheMatrix and cacheSolveMatrix functions with an
### inversable matrix.
makeCacheMatrix(x=matrix(c(3,-1,-5,2), nrow=2))
cacheSolveMatrix(x)

### New matrix values for x, to test if cacheSolveMatrix works well
x <- matrix(c(-2,3,2,4), nrow=2)


