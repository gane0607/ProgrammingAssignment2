## The following functions uses lexical scoping to calculate inverse of a 2 * 2 square
## matrix, store the inverse in a cache memory which can retrive the inverse as long as
## the matrix is not altered

## Creates a empty vector space in the memory so that inverse values can be stored,retrived or reset

makeCacheMatrix <- function(x = matrix()) {
  
                  m <- NULL
                  set <- function(y) {
                         x <<- y             # if x changes its value from previous run
                         m <<- NULL          # if x is reset, so is m to calculate the inverse again
  }
                  get <- function() x       # retrives x from the makeCacheMatrix environment
                  setinverse <- function(inverse) m <<- inverse #caches the value of inverse to m
                  getinverse <- function() m  #gets value of m, maybe a cached value or a new one based on x
                 list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse) #retruns a list containing the return values of get,set,getinverse,setinverse
}                                               # names of the list is same of that of function returns


## The function below takes the matrix as a arugement and looks into the space created by
## makeCacheMatrix to check if there is a inverse value present, if not calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             m <- x$getinverse()      #retrives m from makeCachematrix
             if(!is.null(m)) {
             message("getting cached data")
            return(m)                   #if m exsists for x ,retrives from cache
}
            data <- x$get()             #gets the matrix x,if x is changed
            m <- solve(data, ...)       #calculates inverse and returns m and caches m
            x$setinverse(m)
            m

}
        
      
