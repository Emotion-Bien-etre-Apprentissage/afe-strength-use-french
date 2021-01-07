"sumScoreDf.fct" <-
  function(dataConv.df, param.df) {
    # Mathieu d'Acremont
    # version 1.3
    #
    # first convert data with convScore.fct
    # to handle NA, use replaceNaDf.fct
    # finally use sumScoreDf.fct
    #
    #
    nVar <- length(dataConv.df)
    nCoef <- nrow(param.df)
    if (nVar != nCoef) {
      cat("nbr of coef in param is not equal to nbr of variables\n")
    }
    else {
      nParam <- length(param.df)
      NamParam <- names(param.df)
      nSubj <- nrow(dataConv.df)
      NamSubj <- rownames(dataConv.df)
      res.aa <- NULL
      
      calcScore.fct <- function(data.v, param.v) {
        indexParam.bi <- param.v == -1 | param.v == +1
        sum(data.v[indexParam.bi])
      }

      for (iParam in 1:nParam) {
        param.v <- param.df[,iParam]
        res.v <- apply(dataConv.df, 1, calcScore.fct, param.v)
        res.aa <- c(res.aa, res.v)
      }

      dim(res.aa) <- c(nSubj, nParam)
      rownames(res.aa) <-  NamSubj
      colnames(res.aa) <- NamParam
      as.data.frame(res.aa)
    }
  }
