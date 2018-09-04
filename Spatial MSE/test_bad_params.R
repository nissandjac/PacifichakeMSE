## Fix the hessian problems 
load('modelsave.Rdata')

parameters <- model.save[[2]]$parameters
psm <- model.save[[2]]$xx$BadParams
df.new <- model.save[[2]]$df
yrs <- df.new$years[df.new$selYear]:df.new$years[length(df.new$years)]
nyrs <- length(yrs)
years <- 1966:2017
# Find the parameter group that gives an error 

bad.params <- parameters[psm == 'Bad']

# Structure the parameters correctly 

PSEL <- psm[psm$Param == 'PSEL',]
PSEL1 <- model.save[[1]]$xx$BadParams[model.save[[1]]$xx$BadParams$Param == 'PSEL',]


psel_fish <- parameters[which(names(parameters) == 'psel_fish')]
yr.idx <- seq(1,nyrs*length(psel_fish), by = length(psel_fish))

yrs.sel <- 1991:years[length(years)]
# Sort the PSEL matrix the right way 
PSEL.sorted <-matrix(NA, length(psel_fish),length(yrs.sel))
PSEL.sorted1 <- matrix(NA, length(psel_fish),length(yrs.sel))
PSEL.bad <- matrix(NA, length(psel_fish),length(yrs.sel))

for(i in 1:(length(yr.idx)-1)){
  PSEL.sorted[,i] <- PSEL$MLE[yr.idx[i]:(yr.idx[i+1]-1)]
  PSEL.sorted1[,i] <- PSEL1$MLE[yr.idx[i]:(yr.idx[i+1]-1)]
  PSEL.bad[,i] <- as.character(PSEL$Param_check[yr.idx[i]:(yr.idx[i+1]-1)])
}

colnames(PSEL.sorted) <- yrs.sel
colnames(PSEL.bad) <- yrs.sel

# Check the agee distribtuins in those two years 
plot(df.new$age_catch[,48], type = 'l')
points(model.save[[1]]$df$age_catch[,48])

# Check the parameter estimates for selectivity in those two years

plot(PSEL$MLE)
lines(PSEL1$MLE)

par(mfrow = c(floor(sqrt(length(yrs.sel))), ceiling(sqrt(length(yrs.sel)))), mar = c(2,2,1,1))
for(i in 1:length(yrs.sel)){
hist(PSEL.sorted[,i])  
  
}

