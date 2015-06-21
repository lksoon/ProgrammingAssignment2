## This function calculate the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
  
    get <- function() x
  
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
  
    list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This function calculates the inverse of the matrix created
## by makeCacheMatrix.  It first check if the inverse has been 
## calculated.  If so, it will retrieve the inverse from the
## memory and skip the calculation.  Else, it will calculate 
## inverse of the given matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
  
    data <- x$get()
    
    ## Calculate the inverse matrix
    m <- solve(data)
  
    x$setinverse(m)
    
    ## Returns m
    m
}
