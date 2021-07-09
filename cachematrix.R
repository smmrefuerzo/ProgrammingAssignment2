##The two functions in this program are makeCacheMatrix, makeCacheSolve
##wherein makeCacheMatrix consists of set,get,setinv,getinv
##in here library(MASS) is used in quantifying the inverse for both non squared and square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  k <- NULL                                 ##initializing k as NULL
  set <- function(y) {                      ##setting the matrix
    x <<- y
    k <<- NULL
  }
  get <- function()x                        ##getting matrix x
  setinv <- function(inverse)k <<- inverse  ##setting inverse of the matrix x
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x                               ##acquiring the inverse of the matrix x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve is used in getting the inverse of the programmed matrix of function abovementioned, makeCacheMatrix


cacheSolve <- function(x, ...)                ##getting cache data
{
  k <- x$getinv()
  if(!is.null(k)){                            ##checking whether z is NULL
    message("fetching cached data right now")
    return(k)         #returns inverse value  ##returning inverse value
  }                    
  data <- x$get()                             ##obtaining matrix from dataset
  k <- solve(data, ...)                       ##calculating inverse value by multiplying matrix
  x$setinv(k)                                 ##setting inverse to the dataset
  k                                           ##returning a matrix that is the inverse of 'x'
}

##--------------------------------checking the program here-------------------------------
## s <- makeCacheMatrix(matrix(1:12,3,6))
## s$get()
##     [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]    1    4    7   10    1    4
## [2,]    2    5    8   11    2    5
## [3,]    3    6    9   12    3    6

## s$getinv()
##            [,1]      [,2]       [,3]       [,4]        [,5]      [,6]
## [1,]  0.36585366 0.1951220 0.02439024 -0.1463415  0.36585366 0.1951220
## [2,]  0.19512195 0.1707317 0.14634146  0.1219512  0.19512195 0.1707317
## [3,]  0.02439024 0.1463415 0.26829268  0.3902439  0.02439024 0.1463415
## [4,] -0.14634146 0.1219512 0.39024390  0.6585366 -0.14634146 0.1219512
## [5,]  0.36585366 0.1951220 0.02439024 -0.1463415  0.36585366 0.1951220
## [6,]  0.19512195 0.1707317 0.14634146  0.1219512  0.19512195 0.1707317

## cacheSolve(s)
## fetching cached data right now
##            [,1]      [,2]       [,3]       [,4]        [,5]      [,6]
## [1,]  0.36585366 0.1951220 0.02439024 -0.1463415  0.36585366 0.1951220
## [2,]  0.19512195 0.1707317 0.14634146  0.1219512  0.19512195 0.1707317
## [3,]  0.02439024 0.1463415 0.26829268  0.3902439  0.02439024 0.1463415
## [4,] -0.14634146 0.1219512 0.39024390  0.6585366 -0.14634146 0.1219512
## [5,]  0.36585366 0.1951220 0.02439024 -0.1463415  0.36585366 0.1951220
## [6,]  0.19512195 0.1707317 0.14634146  0.1219512  0.19512195 0.1707317
