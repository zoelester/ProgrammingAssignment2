## Functions to create a special matrix which can cache its inverse, and to calculate & store the inverse of the object created.
## 

## Creates a special matrix which can cache its inverse. 
## Creates a list of set, get, getinverse & setinverse mechanism for this matrix. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set<- function(y){
                x<<-y
                i<<-NULL
                
        }
        get <- function() x
        setinverse <- function (inverse) i <<-inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the special matrix created by makeCacheMatrix. 
## However, it first checks to see if inverse has already be computed. 
## If so, it gets the inverse and skips computation. Otherwise it calculates & caches it for next time. 

cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
