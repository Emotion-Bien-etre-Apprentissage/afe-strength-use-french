# Chargement des libraries----

library(tidyverse) # manipulation selon les règles tidy
library(rio) # pour la conversion sav <-> csv ou/et xlsx

# Acquisition des 6 bdd et conversion----
# en xlsx pour gagner du temps dans la manipulation

for (i in 1:6) {
  a <- paste("data/bdd",i,".sav", sep = "") # fichiers reçus par J. Forest
  b <- paste("data/bdd",i,".xlsx", sep = "") # fichiers préparés pour être compilés à la main.
  convert(a,b)
}

# Conversion du Master xlsx -> csv
convert("data/bdd_master.xlsx", "data/quebec2020_jf_master.csv")

# suite du code----

<<<<<<< Updated upstream
truc.
=======
source("src/anal_val_int_v.2.R")

>>>>>>> Stashed changes
