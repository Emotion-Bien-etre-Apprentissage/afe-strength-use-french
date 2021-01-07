corSemiPartCI.fct <- function(Y, lm.0A, lm.0B) {
  # calc semi partial cor with CI for a predictor of a linear regression
  # A for the full model
  # B for a model with a subset of predictors
  # Mathieu d'Acremont
  # v1.0

  n <- length(Y)
  
  cor(Y, predict(lmA.tmp)) -> rOA
  cor(Y, predict(lmB.tmp)) -> rOB

  rSemi <- sqrt(rA^2 - rB^2)

  VarInf <- (rOA^4 - 2*rOA^2 + 1 - r0B^4 + rOB^2) / n

  lower.tmp <- rSemi - 1.96*sqrt(VarInf)
  upper.tmp <- rSemi + 1.96*sqrt(VarInf)

  res.tmp <- c(rSemi, lower.tmp, upper.tmp)
  res.tmp <- round(res.tmp, 2)
 
  cat(res.tmp, "\n")
}

  
