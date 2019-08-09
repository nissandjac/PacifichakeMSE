## Run the MSE plots 

source('plotMSE.R')

# Plot CLIMATE STUFF
results <- 'results/Climate/'
plotMSE(results,plotnames = c('No change','move little','movemuch'), plotexp = TRUE)

# Plot HCR stuff
results <- 'results/HCR/'
plotMSE(results, plotexp = TRUE, plotnames = c('Floor 50','HCR','JMC','Real'))

results <- 'results/survey/JMC/'
plotMSE(results, plotexp = TRUE, plotnames = c('survey1','survey2','survey3'))

results <- 'results/survey/JTC/'
plotMSE(results, plotexp = TRUE, plotnames = c('survey1','survey2','survey3'))

results <- 'results/survey/Realized/'
plotMSE(results, plotexp = TRUE, plotnames = c('survey1','survey2','survey3'))


# Plot the bias adjustment (no fishing) runs

results <- 'results/bias adjustment/'
plotMSE_biasadjustment(results, plotnames = c('0.87','0.5','0'), plotexp = TRUE)

# Plot HCR stuff
results <- 'results/Selectivity/'
plotMSE(results, plotexp = TRUE)

results <- 'results/Move/'
plotMSE(results, plotexp = FALSE, plotnames = c('Move 1','Move 2','Move 3'))
