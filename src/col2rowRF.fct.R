"col2rowRF.fct" <- function(data.df) {
  # Mathieu d'Acremont
  # version 1.1
   dimDf <- dim(data.df)
   data.aa <- apply(data.df, 2, c)
   dim(data.aa) <- c(dimDf[1] * dimDf[2], 1)

   row.names(data.df) -> rowNam.v
   Subj.fo <- factor(rep(rowNam.v, dimDf[2]))
   Block.v <- as.numeric(gl(dimDf[2], dimDf[1]))

   data.frame(Subj=Subj.fo, Block=Block.v, X=data.aa)
 }
 
