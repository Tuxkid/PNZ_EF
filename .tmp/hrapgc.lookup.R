lookup <-
structure(function(x, y, z = NULL)
{
# Looks up a legend to find the corresponding elements of the input vector
#
# y has all possible values x could take;
# z has all corresponding unabbreviated values of interest
# This function returns those bits of z which correspond to the input values x
# 
# if z is NULL, y must be a matrix of two columns
    if(is.null(z) && dim(y)[2] != 2) stop("z must be specified OR y a matrix\n"
          )
    if(!is.null(dim(y))) {
       z <- y[, 2]
       y <- y[, 1]
    }
    names(z) <- y
    z[x]
}
, comment = "26/01/1999")
