#makeCacheMatrix Function:

           #This function will take the matrix and, furthermore, will get and


           #set the inverse matrix, returning a list of functions and sending 
           #the inverse matrix to cache.


makeCacheMatrix<-function(x = matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

makeCacheMatrix(matrix(1:9,3,3))


#cacheSolve Function:

            #This function will calculate if the inverse of the matrix created in the 

            #function above was cashed, returning the inverse matrix.


cacheSolve<-function(x, ...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<- x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}

#Matrix Created:
test <- matrix(sample(1:10,9),3,3)
print(test)
pass <- makeCacheMatrix(test)
cacheSolve(pass)

