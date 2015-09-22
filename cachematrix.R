## Put comments here that give an overall description of what your
## functions do


#####################################
# test example
#	x<-matrix(nrow=3,ncol=3,c(1:4,1:5) )
# solve(x)
# env_matrix<-makeCacheMatrix(x=x)
# cacheSolve(env_matrix)
# cacheSolve(env_matrix)
#####################################

## make a cache matrix
## will add the matrix to a new env of R
## call setivn to add the inverse of matrix to the same env
## call getinv to get the inverse of matrix from the env
## return a list of the env 
makeCacheMatrix <- function(x = matrix()) {
				invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invm <<- inv
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## read the env list, if the list contain the inverse of matrix
## return the inverse of matrix 
## else  the function will calcute the inverse of matrix and 
## add the inverse of matrix to the env list
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinv()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinv(invm)
        invm
}
