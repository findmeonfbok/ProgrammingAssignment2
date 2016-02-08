## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a list of functions that 
#store and retrieve the matrix, and calculate and retrieve the
##inverse.
##This function is the same as the example given
##as the only changes required were the
##the use of solve instead of mean, and the limiting the argument to a
##matrix

makeCacheMatrix <- function(x = matrix()) {

  inV <- NULL
  set <- function(y) {
    x <<- y
    inV <<- NULL
  }
  get <- function() x
  setInverse <- function(inver) inV <<- inver
  getInverse <- function() inV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)


}


## Write a short comment describing this function

##Ths function takes two arguments:
## The first is a matrix, and the second is the list that 
## has the cache values in it.
##If the inverse has already been solved and the matrix matches
##the one in the cache the stored value is returned.
## if the matrix has changed the new value is calculated and the 
##cache is updated to reflect the changes
## if the value in the cache is null the values are calculated for 
##first time and stored



cacheSolve <- function(x,cacheList, ...)  {

        ## Return a matrix that is the inverse of 'x'
        {
  inV <- cacheList$getInverse()
  data <- cacheList$get()
  if((!is.null(inV)) &&(identical(data,x))) {
    message("getting cached data")
    return(inV)
  }
  if(!(identical(data,x))){
    cacheList$set(x)
    data <- cacheList$get()
  }
  inV <- solve(data, ...)
  cacheList$setInverse(inV)
  inV
}
        
        }


##The following lines will show the code working

myMos<-c(3,5,2,4,1,2,4,8.89,3.99)
mySqMatrix<-matrix(myMos,3,3)
myList<-makeCacheMatrix(mySqMatrix)
cacheSolve(mySqMatrix,myList)
cacheSolve(mySqMatrix,myList)
myMos<-c(3,5,2.4,4,1,2,4,8.89,3.99)
mySqMatrix<-matrix(myMos,3,3)
cacheSolve(mySqMatrix,myList)
