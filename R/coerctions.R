# vector to the representation of attributes of fcaR
to_sparse <- function(Attributes, attributes){
  n <- length(Attributes)
  S <- build_set(attrs = Attributes,
                 values = rep(1,n),attributes)
  return(S )
}

from_sparse <- function(Att1){
  Att1@Dimnames[[1]][which(Att1==1)]
}#end function
