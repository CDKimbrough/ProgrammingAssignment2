## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates the special "matrix" to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     SetInvMat <- function(solve) m <<- solve
     GetInvMat <- function() m
     list(set = set, get = get,
          SetInvMat = SetInvMat,
          GetInvMat = GetInvMat)
}
     


## Write a short comment describing this function
##This function computes the inverse of the matrix created above, returning the cached version
##if it's already been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$GetInvMat()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$SetInvMat(m)
     m
}

##Creating the first matrix
Matrix_A <- makeCacheMatrix(matrix(c(2, 5, -3, 0, 2, 6, 5, 5, 8), nrow=3))

##Solving the matrix inverse
cacheSolve(Matrix_A)
