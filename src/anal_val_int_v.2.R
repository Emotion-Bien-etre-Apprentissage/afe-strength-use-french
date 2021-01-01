# Libraries----

library(mice)
library(psy)
library(psychometric)
library(sem)
library(moments)

# Préparation des fonctions

# Function sumScoreDf----
source(file="C:/Users/pga/switchdrive/R/Work/Lib/RFct/sumScoreDf.fct.R")

# Function convScoreDf----
source(file="C:/Users/pga/switchdrive/R/Work/Lib/RFct/convScoreDf.fct.R")

# Function col2rowRF----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/col2rowRF.fct.R")

# Function corTestDf----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/corTestDf.fct.R")

# Function formCor----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/formCor.fct.R")

# Function ICC.C1----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/ICC.C1.fct.R")

# Function ICC.CK----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/ICC.CK.fct.R")

# Function alphaDf----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/alphaDf.fct.R")

# Function corSemiParCI----
source("C:/Users/pga/switchdrive/R/Work/Lib/RFct/corSemiParCI.fct.R")

# Function suParam----
load(file="C:/Users/pga/switchdrive/R/Work/Lib/RShare/suParam.df")



# Load data----

read.csv(file="quebec2020_jf_master.csv", row.names=1) -> bdd.df
names(bdd.df)
names(bdd.df[15:28])
su.df <- bdd.df[15:28]
names(su.df)

str(su.df)
summary(su.df)

# remove all NA
su.df <- na.omit(su.df) 
summary(su.df)



# compute scores
min(su.df, na.rm=T); max(su.df, na.rm=T)
convScoreDf.fct(su.df, suParam.df$suParamG, 1, 7) -> suQConv.df
sumScoreDf.fct(suQConv.df, suParam.df) -> suVD.df


# descriptives
summary(suQConv.df)
summary(suVD.df)


# item distribution
round(skewness(su.df), 2)
summary(skewness(su.df))

round(kurtosis(su.df), 2)
summary(kurtosis(su.df))

# score G ditribution
round(skewness(suVD.df), 2)
round(kurtosis(suVD.df), 2)


# factorial anal
scree.plot(suQConv.df, sim=100)
scree.plot(suQConv.df, sim=10)
factanal(covmat=cov(suQConv.df), factors=1)

factanal(covmat=cov(suQConv.df), factors=2, rotation = "varimax")
factanal(covmat=cov(suQConv.df), factors=2, rotation = "promax")

factanal(covmat=cov(suQConv.df), factors=3, rotation = "varimax")
factanal(covmat=cov(suQConv.df), factors=3, rotation = "promax")






## reliability

# alpha
round(alphaDf.fct(cov(suQConv.df, use="pairwise"), suParam.df), 2)
round(ICC.CK.fct(su.df, .05), 2)#For the reliability of the scale, the ICC(C,K) formula given by McGraw and Wong (1996) was implemented in R . The ICC(C,K) is equal to the alpha coefficient (Cronbach, 1951) but offer the advantage to have a CI. 


# internal consistency

col2rowRF.fct(su.df) -> suRF.df
cor(su.df) -> su.cor
su.lower <- su.cor[lower.tri(su.cor)]

round(su.cor, 2) # Correlations between all items are positive

corTestDf.fct(su.df, rowSums(su.df)) -> tmp.li
formCor.fct(tmp.li)

#icc(su.df)
mean(su.lower) 
#ICC1.CI(X, Subj, suRF.df) 
#ICC2.CI(X, Subj, suRF.df) 
round(ICC.C1.fct(su.df, .05), 2)#intra-class correlation with 95% IC



