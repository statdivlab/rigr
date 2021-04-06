lsplineD <- function (x, knots, lbl = NULL, version = FALSE)
{
 return(lspline(x=x, knots=knots, lbl=lbl, parameterization="change", version=version))
} 