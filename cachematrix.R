## Put comments here that give an overall description of what your
## functions do

## his function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        inv<-NULL
        set<-function(y)
        {
                x<<-y
                inc<-NULL
        }
        
        get<-function() x
        setInverse<-function(inverse)inv<<-inverse
        getInverse<-function() inv
        list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) 
{
        m <- x$getInverse()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setInverse(m)
        m
}

