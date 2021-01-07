"convScoreDf.fct" <-
  function(data.df, param.v, scoreMin, scoreMax) {
    # Mathieu d'Acremont
    # version 1.2
    inv.bi <- param.v == -1
    if (sum(inv.bi) > 0) {
      scoreMax + scoreMin - data.df[,inv.bi] -> data.df[,inv.bi]
    }
    data.df
  }
