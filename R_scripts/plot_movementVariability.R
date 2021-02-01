# Plot the different viable movement rates

library(TMB)
compile("src/runHakeassessment.cpp")
dyn.load(dynlib("src/runHakeassessment"))
library(r4ss)
library(PacifichakeMSE)
source('R/load_data_seasons.R')
mod <- SS_output('inst/extdata/SS32018/', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
set.seed(123)


nrun <- 10

movemax <- seq(0.1, 0.75, length.out = nrun)
moveout <- seq(0.5,0.85, length.out = nrun)


df <- load_data_seasons(nseason = 4, nspace = 2, catch.future = 0, bfuture = 0.5)


SSBsave <- data.frame(SSB = NA, years = rep(df$years, nrun^2), 
                      run = rep(paste('run', 1:nrun^2, sep = '-'), each = length(df$years)),
                      movemax = NA,
                      moveout = NA,
                      success = NA)
idx <- 1

for(i in 1:nrun){
  for(j in 1:nrun){
    df <- load_data_seasons(nseason = 4, nspace = 2, catch.future = 0, bfuture = 0.5,
                            movemaxinit = movemax[i],
                            moveout = moveout[j])


    sim <- try(run.agebased.true.catch(df))

    SSBsave$SSB[SSBsave$run == paste('run', idx, sep = '-')] <- rowSums(sim$SSB)
    SSBsave$success[SSBsave$run == paste('run', idx, sep = '-')] <- 1
    
    SSBsave$movemax[SSBsave$run == paste('run', idx, sep = '-')] <- movemax[i]
    SSBsave$moveout[SSBsave$run == paste('run', idx, sep = '-')] <- moveout[j]
    # 
    idx <- idx+1
  }
}

SSBsave$move <- paste(SSBsave$movemax, SSBsave$moveout, sep = '-')

p1 <- ggplot(SSBsave, aes(x=  years, y = SSB*1e-6, color = movemax, group = move))+
  geom_line()+theme(legend.position = 'none')+theme_classic()+scale_y_continuous('SSB')+ 
  labs(color='max movement\nrate')
p1

p2 <- ggplot(SSBsave, aes(x=  years, y = SSB*1e-6, color = moveout, group = move))+
  geom_line()+theme(legend.position = 'none')+theme_classic()+scale_y_continuous('SSB')+
  labs(color='return\nrate')
p2

png(filename = 'results/Climate/movement_history.png', height = 12, width = 16, res = 400, units = 'cm')
p1/p2
dev.off()
