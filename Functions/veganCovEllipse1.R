#################################
# FUNCTION: veganCovEllipse1
# packages: vegan
# purpose: make ellipse; code taken from stack exchange
# input:
# output:
# -------------------------------
veganCovEllipse1 <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 1 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
