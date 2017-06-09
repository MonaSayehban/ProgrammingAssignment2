## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Write a short comment describing this function
##  In the folloeing there are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
+     set<-function(y){
+         x<<-y
+         inv<<-NULL
+     }
+     get<- function() x
+     setInverse<- function(inverse) inv <<-inverse
+     getInverse<- function() inv
+     list(set=set,
+          get=get,
+          setInverse=setInverse,
+          getInverse=getInverse)

}


## Write a short comment describing this function
## The following function returns the inverse of the matrix
##If the inverse has already exist,it gets the result, otherwise it computes the inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv<- x$getInverse()
+     if (!is.null(inv)){
+         message("getting cached data")
+         return(inv)
+     }
+     mat<- x$get()
+     inv<-solve(mat,...)
+     x$setInverse(inv)
+     inv}
