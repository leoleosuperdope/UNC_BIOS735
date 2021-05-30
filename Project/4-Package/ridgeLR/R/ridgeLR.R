#' Data Pre-processing for the Design Matrix
#'
#' This function pre-processes the format of the design matrix,
#' specifically converts the categorical variables to the dummy variables
#' using reference cell coding
#'
#' @param X an n by p design matrix which needs pre-process
#'
#' @return the pre-processed design matrix
#'
#' @examples
#'
#' matrix_X(X)
#'
#' @export
matrix_X <- function(X){
  n <- dim(X)[1]
  m <- dim(X)[2]
  df <- rep(1,n)
  for (i in 1:m){
    if (class(X[,i]) %in% c("integer","numeric")) {
      df2 <- X[,i]
      df <- cbind(df,df2)
      colnames(df)[dim(df)[2]] <- colnames(X)[i]
    }
    else if ("" %in% unique(X[,i])){
      l <- length(unique(X[,i]))-1
      df1 <- matrix(0,n,l)
      type <- sort(unique(X[,i]))[! sort(unique(X[,i])) %in% ""]
      for (j in 1:l){
        df1[,j] <- X[,i]==type[j]
      }
      colnames(df1) <- paste(rep(colnames(X)[i],l),type,sep="")
      df <- cbind(df,df1)
    }
    else {
      l <- length(unique(X[,i]))-1
      df1 <- matrix(0,n,l)
      for (j in 1:l) {
        df1[,j] <- X[,i]==unique(X[,i])[j+1]
      }
      colnames(df1) <- paste(rep(colnames(X)[i],l),unique(X[,i])[-1],sep="")
      df <- cbind(df,df1)
    }
  }
  colnames(df)[1] <- "(intercept)"
  df[,-1]
}

#' Fit the logistic regression model
#'
#' This function fits the logistic regression model
#' based on a given vector of outcome variable and a design matrix.
#'
#' @param y an n by 1 matrix containing the outcome variable
#' @param X an n by p design matrix
#'
#' @return the maximum likelihood estimates of the regression coefficients
#'
#' @examples
#'
#' logistic_regression(y, X)
#'
#' @useDynLib ridgeLR
#' @export
logistic_regression <- function(y, X) {
  X <- matrix_X(X)
  logistic_regression_beta(y, X)
}

#' Fit the ridge logistic regression model for a given tuning parameter.
#'
#' This function fits the logistic regression model
#' based on a given vector of outcome variable, a design matrix,
#' and a choice of tuning parameter.
#'
#' @param y an n by 1 matrix containing the outcome variable
#' @param X an n by p design matrix
#' @param lambda a numeric value of the tuning parameter
#'
#' @return the maximum likelihood estimates of the regression coefficients
#'
#' @examples
#'
#' ridge_logistic_regression_beta(y, X, 0.1)
#'
#' @useDynLib ridgeLR
#' @export
ridge_logistic_regression_beta <- function(y, X, lambda) {
  X <- matrix_X(X)
  ridge_logistic_regression_beta2(y, X, lambda)
}

#' Find the optimal value for the tuning parameter of  ridge logistic regression
#' by k-folds cross validation.
#'
#' This function finds the optimal value for the tuning parameter of ridge
#' logistic regression by a given vector of outcome variable, a design matrix,
#' a user-specified sequence of choice of tuning parameter, and the number of
#' folds of the cross validation.
#'
#' @param y an n by 1 matrix containing the outcome variable
#' @param X an n by p design matrix
#' @param lambdas a sequence of values of tuning parameters
#' @param fold number of folds for cross validation
#'
#' @return The optimal tuning parameter for ridge logistic regression by
#' grid search
#'
#' @examples
#'
#' cv_ridge_logistic_regression(y, X, 0:10, 5)
#'
#' @export
cv_ridge_logistic_regression <- function(y, X, lambdas, fold){
  lambdas = lambdas
  CV = numeric()
  for (j in lambdas){
    lambda = j
    index <- sample(1:fold, nrow(y), replace = T)
    resid = 0
    for (i in 1: fold){
      cat(i)
      X.train <- X[index != i, ]
      y.train <- y[index != i]
      X.test <- X[index == i, ]
      y.test <- y[index == i]
      beta <- ridge_logistic_regression_beta(y.train, X.train, lambda)
      resid <- resid + sum((y.test - 1/(1+exp(-X.test%*%beta)))^2)
    }
    CV = c(CV, resid/nrow(y))
  }
  plot(lambdas, CV, type = "b")
  lambda = lambdas[which.min(CV)]
  CVscore = min(CV)
  print(paste0("tunning parameter = ", lambda, "; CV score = ", CVscore))
  return(lambda)
}
