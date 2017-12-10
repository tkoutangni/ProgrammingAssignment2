## This pair of function cache the value of a computed inverse of a matrix A, rather than repeatidely computing it: Extremely usefull especially if the matrix doesn't change often. If the matrix does change, the function comupte and cache the new inverse of the matrix A.
 
## NOTICE : I'm using the function ginv() from MASS package which return Moore-Penrose Generalized Inverse of a given matrix. ginv() returns the inverse of any invertible matrix as opposed to the function "solve" which seems to work only on square matrix.  

## ginv(A) requires loading the MASS package.
## Load MASS package for using the function 
library(MASS)



## The following function named "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

# The object return by this function make it possible to :
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the cached inverse of the matrix

makeCacheMatrix<-function(x = matrix()){
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<- function() x
        setInverse<- function(inverse_new_value) inverse<<-inverse_new_value
        getInverse<- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve  retrieved the inverse from the cache and return it. else it comput the inverse and cache it for future use


cacheSolve<-function(x, ...){
        inverse<-x$getInverse()
        if(!is.null(inverse)){
                message("getting cached inverse matrix value")
                return(inverse)
        }else{
                # compute the inverse of the matrix for the first time
                # and cache it
                data<- x$get()
                inverse<-ginv(data)
                x$setInverse(inverse)
                return(inverse)
        }
}
