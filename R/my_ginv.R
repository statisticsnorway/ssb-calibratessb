

#
# Copy of ginv from package "MASS"
# Only difference: Possibility to return singular values
#
#  + override input by ginv_tol_FromUser
#
my_ginv = function (X, tol = sqrt(.Machine$double.eps),singularReturn=FALSE)
{
  
    tolFromUser <- get0("ginv_tol_FromUser", ifnotfound = FALSE)
    if(tolFromUser){
      if(is.logical(tolFromUser))
        tol = sqrt(.Machine$double.eps)
      else{
        if(is.numeric(tolFromUser))
          tol = tolFromUser
      }
      #cat("ginv-tol set to",tol,"\n")
      #flush.console()
    }
  
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X)))
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X))
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X))
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)

    if (all(Positive))
      invX = Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))  # new: "invX ="
    else if (!any(Positive))
      invX = array(0, dim(X)[2L:1L])            # new: "invX ="
    else invX = Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) *
      t(Xsvd$u[, Positive, drop = FALSE]))    # new: "invX ="
    if(!singularReturn) return(invX)
    list(invX=invX,singular=Xsvd$d)
}
