# Matrix inversion may cost a lot of resource when the matrix is very large
# if we store the result of the inversion then we could just access instead of do two more times
# to inverse the martix again which save a lots resources.

# makeCacheMatrix creates a list containing which is
# really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function calculates the inverse of the matrix created above function.
#It first checks if the inverse has already been computed.
#However, it first checks to see if themean has already been calculated. 
#If so, it `get`s the inverse of the matrix from thecache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix of
#the data and sets the value of the inverse of the matrix in the cache via the `setinverse`
# setinverse function.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
   data <- x$get()
   if (det(data)!=0){
     inverseMatrix <- solve(data)
     x$setinverse(inverseMatrix)
     return (inverseMatrix)    
   }else{
     message("erro:the matrix is a non-vertible matrix")
   }


}

## example output:
# > source('cachematrix.R')
# > x = rbind(c(1, -1/2), c(-1/2, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]  1.0 -0.5
# [2,] -0.5  1.0
# first time run without cache
# > cacheSolve(m)
# [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333
# second time run
# > cacheSolve(m)
# getting cached data.
# [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333

#non-vertible matrix:
# > y <- matrix(1:25,5,5)
# > m = makeCacheMatrix(y)
# > m$get()
# [,1] [,2] [,3] [,4] [,5]
# [1,]    1    6   11   16   21
# [2,]    2    7   12   17   22
# [3,]    3    8   13   18   23
# [4,]    4    9   14   19   24
# [5,]    5   10   15   20   25
# > cacheSolve(m)
# the matrix is a non-vertible matrix