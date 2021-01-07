"alphaDf.fct" <-
  function(qConv.cov, param.df) {
    nCol <- length(param.df)
    alpha.v <- NULL
    for (col in 1:nCol) {
      param.v <- param.df[col]
      iPos <- which(param.v == 1)
      iNeg <- which(param.v == -1)
      i <- c(iPos, iNeg)
      tmp.cov <- qConv.cov[i,i]
      nv <- ncol(tmp.cov)
      alpha <- (nv/(nv-1)) * (1 - sum(diag(tmp.cov)) / sum(tmp.cov))
      alpha.v <- c(alpha.v, alpha)
    }
    alpha.a <- as.array(alpha.v)
    dim(alpha.a) <- c(1, nCol)
    colnames(alpha.a) <- names(param.df)
    rownames(alpha.a) <- "Alpha"
    alpha.a
  }
