## The functions in this file are used to cache the result of Inversion
## of large n x n invertible matrices

## makeCacheMatrix returns a list of functions which can be used to 
## store and retrieve the results on Inverse operation

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<-NULL
        }
        
        get <- function(){
                x                
        }
        
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        getInverse <- function() {
                inv
        }        
        
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## CacheSolve checks to see if the Inverse of a matrix has already been cached
## if so it returns the cahced value. If not it calcuates the inverse and caches the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        if(!is.null(i)){
                message("getting Cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data, ...)
        
        x$setInverse(i)
        
        i
}
