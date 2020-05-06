#' Creates a scatterplot of each pair of covariates.
#' Runs a linear regression of Y on X.
#'
#' Created by Emilios Chrysostomides
#'
#'
#' @param X A matrix of covariates.
#' @param Y A vector of outcomes.
#' @param sub a list of subjects (i.e. a set of integers corresponding to rows in X)
#' @return coef: a set of coefficients from the linear regression of Y on X
#' @return pvals: the set of corrresponding p-values
#' Imports: ggplot2
#' @export
#' @examples
#' myData<- read.csv("myData.csv", header = TRUE)
#' Y <- myData[,1]
#' X <- data.matrix(myData[,-1], rownames.force = NA)
#' myLinearRegression(X, Y)
#' myLinearRegression(X[,-1], Y)
#' myLinearRegression(X[,-1], Y, c(1:5))

myLinearRegression <- function(X, Y, sub = 0){

  if (ncol(X) < 5){
    if (sub == 0){
      full <- cbind.data.frame(X,Y)
      model_fit <- lm(Y ~ ., data = full)
      p.plot <- GGally::ggpairs(full)
      coef <- coefficients(model_fit)
      pvals <- summary(model_fit)$coefficients[,4]
      return(list(p.plot, coef, pvals))
    }
    else{
      full <- cbind.data.frame(X[sub,],Y)
      model_fit <- lm(Y ~ ., data = full)
      p.plot <- GGally::ggpairs(full)
      coef <- coefficients(model_fit)
      pvals <- summary(model_fit)$coefficients[,4]
      return(list(p.plot, coef, pvals))
    }
  }
  else {
    print("Too many variables to plot")
  }

}
