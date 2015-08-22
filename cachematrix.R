## a pair of functions that cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse

## argument 'x' is original matrix to be calculated
makeCacheMatrix <- function(x = matrix()) {
        
        # empty storage for inversematrix 
        inversematrix <- NULL           
        
        # store inversematrix after solve calculation or whatever outside do
        storeInvMtrx <- function(invmtrx){
                inversematrix <<- invmtrx
        }
        
        # get inversematrix for cache
        getInvMtrx <- function() inversematrix
        
        # get original matrix for solve calculation 
        # original matrix is argument 'x' in this function
        getOriMtrx <- function() x
        
        # renew original matrix (inversematrix needed to be reset when renewing original)
        # original matrix is argument 'x' in this function
        renewOriMtrx <- function(newmtrx){
                inversematrix <<- NULL
                x <<- newmtrx
        }
        
        # return list of functions to be used outside. name every elements to use readily
        list(storeInvMtrx = storeInvMtrx, getInvMtrx = getInvMtrx, 
             getOriMtrx = getOriMtrx, renewOriMtrx = renewOriMtrx )
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## argument 'x' is special matrix object(consisting of original matrix and
## related functions) made already

cacheSolve <- function(x, ...) {
        
        # get cached inversematrix
        inversematrix <- x$getInvMtrx()
        
        # if not calculated so that no value for inversematrix,
        # calculate and assign
        if(is.null(inversematrix)){
                originalmatrix <- x$getOriMtrx()
                inversematrix <- solve(originalmatrix)
                x$storeInvMtrx(inversematrix)
        
        # if cacluated already and assigned,
        # just get the cached inversematrix
        }else{
                message ("getting cached data")
        }
        
        ## Return a matrix that is the inverse of 'x'
        return(inversematrix)
}
