# The point of the week 3 programming assignment is to learn about lexical 
#  scoping in R. 

makeCacheMatrix <- function(x = matrix()) { #initializing the default function arg as empty matrix
# create a special "matrix" object that can cache its inverse.
  s <- NULL  # initializing within the local env 
    set <- function(y) {  #assigns the input arg to x of parent env 
       x <<- y            #local env y is being assigned to parent env x
       s <<- NULL         #clears (the cache of) any prior value of the 
                          #parent env object s
    }
    get <- function() x   #Since x is not defined within get(), R gets it from the parent env 
    setsolve <- function(solve) s <<- solve #defines the setter, since s 
                                            #is defined in the parent env 
                                            #we need the <<- operator

    getsolve <- function() s #Since s is not defined within getsolve(), R gets it from the parent env 
    list(set = set, get = get,  #assign each of these functions as an 
         setsolve = setsolve,   #element within a list(), and return 
         getsolve = getsolve)   #it to the parent environment.
}


#  This function computes the inverse of the special "matrix" returned 
#  by makeCacheMatrix above. If the inverse has already been calculated 
#  (and the matrix has not changed), then the cachesolve should retrieve 
#  the inverse from the cache.

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x',  cachesolve() is 
# required to populate and/or retrieve the inverse from an object 
# of type makeCacheMatrix().
  
     s <- x$getsolve()    #get the Imatrix from the passed arg
    if(!is.null(s)) {     #if s not equal to NULL, we have a valid, cached 
                          #mean and can return it to the parent env
      message("getting cached data")
      return(s)
    }
    data <- x$get()       #get the matrix from the input object, 
    s <- solve(data, ...) #calc the Imatrix, 
    x$setsolve(s)         #set the Imatrix in the input object, 
    s                     #return the Imatrix to the parent env
}
