{
  
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