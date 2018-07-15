## Cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##1. setvalue of matrix; 2. get value of matrix; 3. set value of inversion; 4. get value of inversion.


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinversion<-function(solve)m<<-solve
        getinversion<-function()m
        list(set=set,get=get,
             setinversion=setinversion,
             getinversion=getinversion)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversion()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m
}

## This is for testing my function.
test_matrix<-matrix(c(1,2,3,4),nrow=2)
my_matrix<-makeCacheMatrix(test_matrix)
cacheSolve(my_matrix)