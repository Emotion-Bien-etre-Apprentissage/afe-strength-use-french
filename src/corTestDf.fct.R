corTestDf.fct <- function(first.tmp, second.tmp, method="pearson", subset=NULL) {
    # Mathieu d'Acremont, Mathieu.Dacremont@pse.unige.ch
    # University of Geneva
    # version 1.0

    first.df <- as.data.frame(first.tmp)
    second.df <- as.data.frame(second.tmp)

    if (nrow(first.df) != nrow(second.df)) {return("Data frames have different # of rows. Please check")} # exit

    varNam1 <- names(first.df)
    varNam2 <- names(second.df)

    if (length(subset) != 0){ # a subset is given
      if(length(subset) != nrow(first.df)) {return("Subset do not equal the # of rows in the data frames. Please check")} # exit
      first.df <- as.data.frame(first.df[subset,])
      second.df <- as.data.frame(second.df[subset,])
    }


    l1 <- length(first.df)
    l2 <- length(second.df)

    index1 <- 1:l1
    index2 <- 1:l2
    
    cor.ma <- matrix(999, l1, l2)
    corLower.ma <- matrix(999, l1, l2)
    corUpper.ma <- matrix(999, l1, l2)
    df.ma <- matrix(999, l1, l2)
    t.ma <- matrix(999, l1, l2)
    p.ma <- matrix(999, l1, l2)

    for (i1 in index1) {
      for (i2 in index2) {
        v1 <- first.df[,i1]
        v2 <- second.df[,i2]
        
        nbrEqual <- sum(v1 == v2, na.rm=T)
        lv1 <- sum(v1 == v1, na.rm=T)
        lv2 <- sum(v2 == v2, na.rm=T)
        if (nbrEqual == max(lv1, lv2)) {
          cor.ma[i1,i2] <- NA
          corLower.ma[i1,i2] <- NA
          corUpper.ma[i1,i2] <- NA
          t.ma[i1,i2] <- NA
          df.ma[i1,i2] <- NA
          p.ma[i1,i2] <- NA
        }
        else {
          if (is.factor(v1)) {v1 <- as.numeric(v1)}
          if (is.factor(v2)) {v2 <- as.numeric(v2)}
          test <- cor.test(v1, v2, alternative="two.sided", method=method)
          if (method == "pearson") {
            cor.ma[i1,i2] <- test$estimate
            corLower.ma[i1,i2] <- test$conf.int[1]
            corUpper.ma[i1,i2] <- test$conf.int[2]
            df.ma[i1,i2] <- test$parameter
            t.ma[i1,i2] <- test$statistic
            p.ma[i1,i2] <- test$p.value
          }
          else {
            cor.ma[i1,i2] <- test$estimate
            corLower.ma[i1,i2] <- NA
            corUpper.ma[i1,i2] <- NA
            df.ma[i1,i2] <- NA
            t.ma[i1,i2] <- NA
            p.ma[i1,i2] <- test$p.value
          }
            
        }
      }
    }
    rownames(cor.ma) <- varNam1
    colnames(cor.ma) <- varNam2

    rownames(corLower.ma) <- varNam1
    colnames(corLower.ma) <- varNam2

    rownames(corUpper.ma) <- varNam1
    colnames(corUpper.ma) <- varNam2

    rownames(t.ma) <- varNam1
    colnames(t.ma) <- varNam2

    rownames(df.ma) <- varNam1
    colnames(df.ma) <- varNam2

    rownames(p.ma) <- varNam1
    colnames(p.ma) <- varNam2

    list(r=cor.ma, lower=corLower.ma, upper=corUpper.ma, t=t.ma, df=df.ma, p=p.ma)
}

