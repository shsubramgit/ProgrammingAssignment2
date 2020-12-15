
#-------------------------------------------------------------------------------------------
#       Coursera:  R Programming : Assignment :  Lexical Scoping / Caching
#-------------------------------------------------------------------------------------------

# Set matrix
makeMatrix  <-  function(x = matrix()) {
  
        inv <- NULL               # inverse not yet computed
        set <- function(y) {
                  x <<- y
                  inv <<- NULL   
        }
          
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)   # function output is a List(,)
}

#-------------------------------------------------------------------------------------------

# Solve for matrix inverse

cacheInverse   <-  function(x, ...) {
  
           inv <- x$getInverse()
           
              if (!is.null(inv)) {
                  message("getting cached data")  # if already solved, get from cache
              return(inv)
              }
           
          A <- x$get()             # get input data and set it to a matrix
          A_inv <- solve(A, ...)   # solve() computes matrix inverse
          x$setInverse(A_inv)
          A_inv                    # output the inverse
}

#-------------------------------------------------------------------------------------------

