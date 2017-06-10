## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Matrix inversion is usually a costly computation and there may be some benefit to caching 
##the inverse of a matrix rather than compute it repeatedly.
##makeCacheMatrix creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # cached inverse of matrix

inv<-NULL
+     set<-function(y){
+         x<<-y
+         inv<<-NULL
+     }
+     get<- function() x  ## getter for matrix
+     setInverse<- function(inverse) inv <<-inverse
+     getInverse<- function() inv
+     list(set=set,
+          get=get,
+          setInverse=setInverse,
+          getInverse=getInverse)    ## return list of functions for matrix

}


## Write a short comment describing this function
##The following function returns the inverse of the matrix.
##If the inverse has already been computed, it gets the result. Otherwise, it computes the inverse,
# sets the value in the cache via setInverse function.


cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
        inv<- x$getInverse()
        # return cached matrix inverse if it's been already computed
+     if (!is.null(inv)){
+         message("getting cached data")      
+         return(inv)
+     }
          # compute inverse of matrix 
+     mat<- x$get()
+     inv<-solve(mat,...)
+     x$setInverse(inv)
+     inv
        
}

###Test
my_matrix<- makeCacheMatrix(matrix(1:4,2,2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
