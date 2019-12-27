## Run the MSE plots 

source('plotMSE.R')

# Plot CLIMATE STUFF
results <- 'results/Climate/'
plotnames <- c('No change','medium increase','high increase')
#plotnames <-  factor(plotnames, levels = c('No change','Medium increase', 'High increase'))

#plotnames <- c('1','2','3')
plotMSE(results,plotnames = plotnames, plotexp = FALSE)

# Plot HCR stuff
results <- 'results/HCR/'
plotMSE(results, plotexp = TRUE, plotnames = c('Floor 50','HCR','Historical TAC','Realized'))

results <- 'results/survey/JMC/'
plotMSE(results, plotexp = TRUE, plotnames = c('survey1','survey2','survey3'))

results <- 'results/survey/JTC/'
plotMSE(results, plotexp = TRUE, plotnames = c('survey1','survey2','survey3'))

results <- 'results/survey/Realized/'
plotMSE(results, plotexp = FALSE, plotnames = c('survey1','survey2','survey3'))


# Plot the bias adjustment (no fishing) runs
source('plotMSE_biasadjustment.R')
results <- 'results/bias adjustment/'
plotMSE_biasadjustment(results, plotnames = c('0.87','0.5','0'), plotexp = TRUE)

# Plot HCR stuff
results <- 'results/Selectivity/'
plotnames <-  c('Conditioned \nmodel','US small \nselectivity','2018 selectivity')
#plotnames <- c('1','2','3')
plotMSE(results,plotnames = plotnames, plotexp = TRUE)
