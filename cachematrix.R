## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special matrix object that cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                                          inv <- NULL
                                          set <- function(y) {
                                          x <<- y
                                          inv <<- NULL
                                                        }
                                          get <- function() x
                                          setInverse <- function(inverse) inv <<- inverse
                                          getInverse <- function() inv
                                          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
                                          }


## Write a short comment describing this function
## The fuction calculates the inverse of the special matrix created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then it should bring back the inverse for the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
                                inv <- x$getInverse()
                                if (!is.null(inv)) {
                                message("getting cached data")
                                return(inv)
                                }
                                dados <- x$get()
                                inv <- solve(dados)
                                x$setInverse(inv)
                                inv
                                }

## Example
## my_matrix <- makeCacheMatrix(matrix(11:14, 2, 2))
## my_matrix$get()
##     [,1] [,2]
##[1,]   11   13
##[2,]   12   14
## my_matrix$getInverse()
##NULL
## cacheSolve(my_matrix)
##     [,1] [,2]
##[1,]   -7  6.5
##[2,]    6 -5.5
