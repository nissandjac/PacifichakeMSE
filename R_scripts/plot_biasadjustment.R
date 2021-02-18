# Plot bias adjustment
library(TMB)
library(r4ss)
library(PacifichakeMSE)

compile("src/runHakeassessment.cpp")
dyn.load(dynlib("src/runHakeassessment"))
mod <- SS_output('inst/extdata/SS32019/', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
set.seed(123)


nyrs <- 50
df <- load_data_seasons(nseason = 4, nspace = 2, yr_future = nyrs, catch.future = 0, bfuture = 0.5)

nruns <- 1000
seedz <- round(runif(nruns,min = 1, max = 10000))

bs <- round(seq(0,1, length.out = 10), digits = 1)

SSBsave <- data.frame(SSB = NA, years = rep(df$years,nruns*length(bs)),
                      run = rep(paste('run', 1:nruns, sep = '-'), each = df$nyear*length(bs)),
                      b = rep(as.character(bs), each = df$nyear))

for(i in 1:nruns){
  for(j in 1:length(bs)){

    df <- load_data_seasons(nseason = 4, nspace = 2, yr_future = nyrs, catch.future = 0, bfuture = bs[j])
    sim.tmp <- run.agebased.true.catch(df, seedz[i])
    runidx <- paste('run',i, sep = '-')
    SSBsave$SSB[which(SSBsave$run == runidx & SSBsave$b == as.character(bs[j]))] <- rowSums(sim.tmp$SSB)


  }


}

SSB.tot <- SSBsave %>%
  group_by(years,b) %>%
  summarise(SSBmed = median(SSB),
            SSBmin = quantile(SSB, probs = 0.05),
            SSBmax = quantile(SSB, probs = 0.95))

cols <- PNWColors::pnw_palette('Starfish', n = length(bs), type = 'discrete')

p1 <- ggplot(SSB.tot, aes(x = years, y = SSBmed*1e-6, color = b))+geom_line(size = 1.2)+theme_classic()+
  geom_ribbon(aes(ymin = SSBmin*1e-6, ymax = SSBmax*1e-6), fill = 'gray', alpha = 0.2, linetype = 0)+
  geom_hline(aes(yintercept = sum(sim.tmp$SSB0)*1e-6), linetype = 2, color = 'black', size = 0.8)+
  #scale_color_manual(values = cols)+
  scale_y_continuous('SSB')+theme(legend.position = c(0.1,0.8))



png(filename = 'results/Climate/biasadjustment_ssb.png', width = 16, height = 10, res = 400, units = 'cm')
p1
dev.off()



