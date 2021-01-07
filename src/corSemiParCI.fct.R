corSemiParCI.fct <- function(Y, lm.0A, lm.0B, sign=+1) {
  # calc semi partial cor with CI for a predictor of a linear regression
  # A for the full model
  # B for a model with a subset of predictors
  # Mathieu d'Acremont
  # v1.0

  n <- length(Y)
  
  cor(Y, predict(lmA.tmp)) -> rOA
  cor(Y, predict(lmB.tmp)) -> rOB

  rSemi <- sqrt(rOA^2 - rOB^2)

  VarInf <- (rOA^4 - 2*rOA^2 + 1 - rOB^4 + rOB^2) / n

  lower.tmp <- rSemi - 1.96*sqrt(VarInf)
  upper.tmp <- rSemi + 1.96*sqrt(VarInf)

  if (sign==1) {
    res.tmp <- c(rSemi, lower.tmp, upper.tmp)
  }
  else {
    res.tmp <- c(-rSemi, -upper.tmp, -lower.tmp)
  }

  names(res.tmp) <- c("r", "lower", "upper")
  return(res.tmp)
}

  
