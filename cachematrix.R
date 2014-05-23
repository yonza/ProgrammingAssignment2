
##################makeCacheMatrix()##################
## returns a list of four functions: one that caches a matrix,
## another that retrieves the cached matrix, another that
## caches the inverse of that matrix, another that retrieves
## the inverse of that matrix
#####################################################

makeCacheMatrix <- function(x = matrix()) {
  
  inv<<-NULL #create and set inv to NULL
  
  set <- function(a){ #caches a matrix in variable 'b'
    b<<-a
    inv <<- NULL #resets the value of inv
  }
  get <- function()b #returns the current cached matrix
  
  setInverse <- function(y)  inv <<- y #caches inv matrix
  
  getInverse <- function() inv #gets cached inv matrix
  
  list(set=set,get=get,setInverse = setInverse, 
       getInverse=getInverse) #returns a list of functions
  
}

################cacheSolve()############################
## cacheSolve takes a list of functions created by makeCacheMatrix
## and pulls out the inversion of the matrix cached in 
## variable 'b'.  If this inverse has already been calculated
## and cached it will just pull it from the cache rather than
## recalculating it
####################################################

cacheSolve <- function(x, ...) {
  inv <- x$getInverse #takes current cached value of inv
  if (!is.null(inv)){ #determines if value already cached
    message("getting cached data”)
    return(inv) #if value is cached the function returns value
  }
  
  myMatrix <- x$get() #gets the value of matrix in question
  inv <- solve(myMatrix) #inverts matrix
  x$setInverse(inv) #caches the inverse
  inv #returns the inverse
  
}