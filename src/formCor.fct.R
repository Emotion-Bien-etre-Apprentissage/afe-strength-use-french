formCor.fct <- function(cor.li, digits=2) {
  # format correlations returned by cor.test.fct
  r.v <- formatC(cor.li$r, digits, format="f")
  lower.v <- formatC(cor.li$lower, digits, format="f")
  upper.v <- formatC(cor.li$upper, digits, format="f")

  cor.li$p -> p.v
  rep("", length(p.v)) -> sig.v
  sig.v[p.v < .05] <- "*"

  res.v <- paste(r.v, sig.v , " (", lower.v, " ", upper.v, ")", sep="")
#  replace(res.v, res.v == " NA ( NA  NA)", " NA") -> res.v
  gsub("0\\.", ".", res.v) -> res.v
  dim(res.v) <- dim(r.v)
  colnames(res.v) <- colnames(r.v)
  rownames(res.v) <- rownames(r.v)
  cat("\nCorrelations within CI")
  cat("\n\n")
  print(res.v, quote=F)
}
  
