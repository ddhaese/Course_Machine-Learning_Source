"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


# Description:
#  In density plots, the densities sometimes extents outside the domain of a 
#  variable. This functions takes the densities that fall outside the domain,
#  'mirrors' them across the respective boundary and sums the reflected portions
#  of the densities with the density values that fall already within the domain.
# Usage:
#   Percentages <- runif ( 500, 0, 1 )
#   Percentage %>% density %>% Mirror ( 0:1 ) %>% plot
mirror <- function ( iData, iCuts ){
  X <- iData$x
  Y <- iData$y
  N <- length ( X )
  
  H <- which ( X < min ( iCuts ))
  Nh <- length ( H )
  
  if ( Nh > 0 ){
    Y [( 2 * Nh ):( Nh + 1 )] <- Y [( 2 * Nh ):( Nh + 1 )] +
      Y [ H ]
    Y [ H ] <- 0
  }
  
  TT <- which ( X > max ( iCuts ))
  Nt <- length ( TT )
  
  if ( Nt > 0 ){
    Y [( N - Nt ):( N - 2*Nt + 1)] <- Y [( N - Nt ):( N - 2*Nt + 1)] +
      Y [ TT ]
    Y [ TT ] <- 0
  }
  
  iData$x <- X
  iData$y <- Y
  
  return ( iData )
}