## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		# creates the object
        m <- NULL
        setfunc <- function(y) {
                x <<- y
                m <<- NULL
        }
        getfunc <- function() x
		
		#sets data in the cache
        setsolve <- function(solve) m <<- solve
		
		#gets data
        getsolve <- function() m
        list(set = setfunc, get = getfunc,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
##computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		#gets matrix 
		m <- x$getsolve()	
		
		#if the inverse has already been calculated (not null)
        if(!is.null(m)) {				
                message("getting cached data")
				#skips the computation
				#and returns cached matrix inverse using previously computed matrix inverse
                return(m)
        }
        data <- x$get()
		
		#calculates the inverse
        m <- solve(data, ...)
		
		#sets the inverse in the cache
        x$setsolve(m)
		
		#outputs matrix
        m
}
