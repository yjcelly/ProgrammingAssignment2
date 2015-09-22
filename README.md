#  Caching the Inverse of a Matrix

makeCacheMatrix��  This function creates a special "matrix" object that can cache its inverse.
cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



# test example
	x<-matrix(nrow=3,ncol=3,c(1:4,1:5) )
 solve(x)
 env_matrix<-makeCacheMatrix(x=x)
 cacheSolve(env_matrix)
 cacheSolve(env_matrix)
