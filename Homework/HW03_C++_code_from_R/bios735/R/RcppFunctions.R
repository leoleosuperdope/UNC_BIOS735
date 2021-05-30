#' @useDynLib bios735
#' @export
one.or.exp <- function(x) {
  one_or_exp(x)
}

#' @export
randomWalk2 <- function(niter, lambda) {
  randomWalk2Rcpp(niter, lambda)
}

#' @export
armadilloSolve <- function(A, b) {
  armadillo_solve(A, b)
}

#' @export
colRidge2 <- function(Y, X, lambda) {
  col_ridge_2(Y, X, lambda)
}
