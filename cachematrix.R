## R Programming Assignment 2: Lexical Scoping
## Caching the inverse of a matrix

## makeCacheMatrix function creates a list, which containes functions
## to set and get the value of the matrix
## and to set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv_m <- NULL                                     #define the inverse as NULL for the first call
  set <- function(y) {                              #function to modify an original matrix x
    x <<- y                                         
    inv_m <<- NULL                                  #as original matrix was changed we should recompute the inverse
  }
  get <- function() x                               #function to return matrix x
  set_inv_m <- function(solve) inv_m <<- solve      #function to modify the inverse of matrix x
  get_inv_m <- function() inv_m                     #function to return the inverse of matrix x
  list(set = set, get = get,                        #the list of functions to return by the makeCacheMatrix
       set_inv_m = set_inv_m,                       #names of the list are the same as the names of functions
       get_inv_m = get_inv_m
       )
}

## cacheSolve function computes the inverse of the matrix using the environment of makeCacheMatrix
## function as a cache. The function computes an inverse of the matrix only at first call and if 
## the matrix was changed since the last call.  Otherwise it returns the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv_m <- x$get_inv_m()                            #get an inversed matrix from cache
  if(!is.null(inv_m)) {                             #if the inverse is not NULL, 
    message("getting cached data")
    return(inv_m)                                   #just return it from cache
  }                                                 #if there is no inverse in the cache, 
  data <- x$get()                                   #we get the matrix, 
  inv_m <- solve(data, ...)                         #compute the inverse,
  x$set_inv_m(inv_m)                                #save inverse to the cache
  inv_m                                             #and return computed inverse
  
}