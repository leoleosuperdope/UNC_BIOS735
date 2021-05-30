#' Calculates the sample variance for each row in a matrix
#'
#' This function calculates the sample variance
#' for each row in a matrix.
#'
#' @param x a matrix with m rows and n columns
#'
#' @return m sample variances for m rows of the matrix
#'
#' @examples
#'
#' m <- 5
#' n <- 50
#' little.n <- n/2
#' set.seed(1)
#' x <- matrix(rnorm(m*n),nrow=m,ncol=n)
#' rowVars(x)
#'
#' @export
rowVars <- function(x) {
  Var <- rowSums((x-rowMeans(x))^2) / (ncol(x)-1)
  return(Var)
}

#' Calculates the t-test statistic for each row in a matrix
#'
#' This function calculates the two-sample t-test statistic (equal variance)
#' for each row in a matrix based on a given factor with two levels.
#'
#' @param x a matrix with m rows and n columns
#' @param f a factor with 2 levels with a total length of n indicating
#' the group assignments for the n elements in a row
#'
#' @return m t-test statistics for m rows of the matrix
#'
#' @examples
#'
#' m <- 5
#' n <- 50
#' little.n <- n/2
#' set.seed(1)
#' x <- matrix(rnorm(m*n),nrow=m,ncol=n)
#' f <- gl(2,little.n)
#' getT(x, f)
#'
#' @export
getT <- function (x, f){
  if (nlevels(f)==2){
    xbar.diff <- rowMeans(x[, which(f==1)]) - rowMeans(x[, which(f==2)]) # calculate the difference in row means
    n1 <- sum(f==1) # sample size in group 1
    n2 <- sum(f==2) # sample size in group 2
    sp.sq <- ((n1-1)*rowVars(x[, which(f==1)])+(n2-1)*rowVars(x[, which(f==2)]))/(n1 + n2 - 2) #s_p squared
    ts <- xbar.diff/sqrt(sp.sq*(1/n1 + 1/n2)) # output a vector of t test statistics
  }
  if (nlevels(f)!=2){
    stop("the number of levels of f should be 2")
  }
  if (length(f[which(f==1)])!=length(f[which(f==2)])){
    stop("two groups do not have equal sample size")
  }
  return(ts)
}
