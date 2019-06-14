## Test selectivity calculations 

library(r4ss)
library(TMB)
source('getSelec.R')

mod <- SS_output(paste(getwd(),'/data/SS32018', sep =''), printstats=FALSE, verbose = FALSE)

pars <- mod$parameters[,c("Value","Active_Cnt")]
pars <- pars[is.na(pars$Active_Cnt) == 0,] # Remove parameters that are not estimated
nms <- rownames(pars)


psel <- pars$Value[80:84] # Naming conventions in SS3 are a bit annoying to index with grep here

# Time varying selectivity parms
fish_ages <- c('P3','P4', 'P5', 'P6', 'P7')
PSEL <- matrix(0,5, length(1991:2017))

for(i in 1:length(fish_ages)){ # Generalize later
  idx <- grep(paste('AgeSel_',fish_ages[i],'_F',sep = ''), nms)
  
  nms[idx[2:length(idx)]] # First one is not a deviation
  PSEL[i,] <- pars$Value[idx[2:length(idx)]]
}
colnames(PSEL) <- 1991:2017

age <- 0:20

# Get baseline selectivity 
sel0 <- getSelec(age = age,psel = psel, Smin = 1, Smax = 6)
selidx <- which(mod$ageselex$Yr == 1966 & mod$ageselex$Factor == 'Asel' & mod$ageselex$Fleet == 1)
sel0ss <- mod$ageselex[selidx,paste(age)]

plot(age,sel0)
lines(age,sel0ss, type = 'l', col = 'red')

# Time varying selectivity 
year<- 1965:2018

selyear <- 2017

sel_year <- getSelec(age = age,psel = psel+PSEL[colnames(PSEL) == (selyear)]*1.4, Smin = 1, Smax = 6)

selidx <- which(mod$ageselex$Yr == selyear & mod$ageselex$Factor == 'Asel' & mod$ageselex$Fleet == 1)
sel_year_ss <- mod$ageselex[selidx,paste(age)]

plot(age,sel_year, ylim = c(0,1.2))
lines(age,sel_year_ss, col = 'red')
