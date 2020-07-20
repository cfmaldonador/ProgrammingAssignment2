## makeCacheMatrix & cacheSolve by Cosme Maldonado (2020)
## you can use this code freely under the only condition
## of giving credit for the author.
## makeCacheMatrix & cacheSolve por Cosme Maldonado (2020)
## este código puede ser usado libremente con la única condición
## de dar crédito al autor.


## This is an exercise for Coursera's R programming course
## This functions intend to speed processes involving getting several times
## same value for inverted matrixes

## First Function is intended for management of the inverted matrix, 
## preparing, caching, solving and retrieving the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inversa<-NULL  ## Inits solution to null 
  prepara<-function(y){ 
    x<<-y 
    inversa<<-NULL ## If a new value has to be calculated cache is reset to NULL
  }
  toma<-function() x 
  invierte<-function(solve) inversa <<-solve
  recupera<-function() inversa
  ## Function returns four methods
  list(prepara=prepara,toma=toma,invierte=invierte,recupera=recupera) 
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inversa<-x$recupera() ## tries to get a cached value 
  if(is.null(inversa)){ ## if cached value does not exist make calculation
    datos<-x$toma()
    inversa<- solve(datos, ...)
    x$prepara(inversa) ## and cache it
  }
  inversa ## function result is inverted matrix
}
## I believe that my approach to cache solve is mor efficient than 
## Roger Peng's cachemean because my function doesn't have to break the 
## function execution with return