ICC.CK.fct <- function(data.df, alpha) {
  # calculate the intra-class corelation, consistency version
  # data.df has the subj in row and the repeated measure in col
  # subj represent a random gouping factor

  data.ma <- as.matrix(data.df)
  
  n <- nrow(data.ma) # nbr of subj
  k <- ncol(data.ma) # nbr of repeated measures
  DFr <- n - 1
  DFe <- (n - 1)*(k - 1)

  Y.. <- mean(data.ma)
  Yi. <- apply(data.ma, 1, mean)
  Y.j <- apply(data.ma, 2, mean)

  # similar as aov(X ~ Subj + as.factor(Block), data=dataRF.df)
  SSr <- 0 # Sum of Square, Between row
  SSe <- 0 # Error
  for (i in 1:n) { # go through row
    Square <- (Yi.[i] - Y..)^2
    SSr <- SSr + Square
    for (j in 1:k) { # go through col
      Square <- (data.ma[i,j] - Yi.[i] - Y.j[j] + Y..)^2
      SSe <- SSe + Square
    }
  }
  SSr <- k*SSr

  MSr <- SSr / DFr # Mean Square, Between row
  MSe <- SSe / DFe # Error
  Fobs <- MSr / MSe

  FLtab <- qf(1-alpha/2, DFr, DFe)
  FUtab <- qf(1-alpha/2, DFe, DFr)

  FL <- Fobs / FLtab
  FU <- Fobs * FUtab

  ICC <- (MSr - MSe) / MSr

  lowICC <- 1 - 1 / FL
  upICC <- 1 - 1 /FU

  sol <- c(lowICC, ICC, upICC)
  names(sol) <- c("lower", "ICC(C,k)", "upper")

  return(sol)
    
}
