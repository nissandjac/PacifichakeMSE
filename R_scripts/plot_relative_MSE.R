# Plot the climate MSE climate results
library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)
library(purrr)
library(dplyr)
library(reshape2)
library(patchwork)

mod <- SS_output('inst/extdata/SS32018', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
seedz <- 12345
set.seed(seedz)

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5) # Prepare data for operating model

parms.true <- getParameters_OM(TRUE,mod, df) # Load parameters from assessment

time <- 1
yrinit <- df$nyear
nruns <- 500

seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE
### Set targets for harvesting etc
#

simyears <- 30 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018

simdata0 <- sim.data # The other one is gonna get overwritten.



#
# folder <- 'C:/Users/Nis/Dropbox/NOAA/Hake MSE/MSE results/final results/'
#
folder <- 'C:/Users/nsja/Dropbox/NOAA/Hake MSE/MSE results/final results/'
#


files <- dir(folder)[grep(dir(folder),pattern = '.Rdata')]

# Recreate the violin pltos

yrs <- 1966:2047

perc <- c(0.25,0.75) # Percentiles for plotting

load_all()

for(i in 1:length(files)){
  load(paste(folder,files[i], sep = '')) # Prints as 'ls.save'
  df.MSE <- flatten(ls.save) # Change the list a little bit
  
  # ProcessMSE is in the df_MSE function 
  catchcdf <- processMSE(df.MSE, 'Catch', idx = c(2,3), spacenames = c('CAN', 'USA'), runs = 500,nspace = 2)
  catchdf <- processMSE(df.MSE, 'Catch', idx = 2, spacenames = c('value'), runs = 500,nspace = 2)
  
  SSB_mid <- processMSE(df.MSE, id = 'SSB.mid', idx = c(1,2), spacenames = c('CAN', 'USA'), runs = 500,nspace = 2)
  SSB_tot <- processMSE(df.MSE, id = 'SSB', idx = 1, spacenames = c('value'), runs = 500,nspace = 2)
  
  SSB.om <- processMSE(df.MSE, id = 'SSB.hes', idx = 1, fn = 'rows', spacenames = c('value'), runs = 500,nspace = 2)
  
  # Calculate AAV
  AAVdf <- AAV(catchcdf, idx = 4)
  AAVtottmp <- AAV(catchdf, idx = 2)
  
  
  
  catch.tot.tmp <- catchdf %>%
    group_by(year) %>%
    summarise(catchmean = median(value),
              quants95 = quantile(value, probs =perc[2]),
              quants5 = quantile(value, probs= perc[1]))
  
  
  SSB.tot.tmp <- SSB_tot  %>%
    group_by(year) %>%
    summarise(SSBmean = median(value),
              quants95 = quantile(value, probs = perc[2]),
              quants5 = quantile(value, probs= perc[1]))
  
  
  AAV.tot.tmp <- AAVtottmp[AAVtottmp$year > 1966,] %>%
    group_by(year) %>%
    summarise(AAVmean = median(AAV, na.rm = TRUE),
              quants95 = quantile(AAV, probs = perc[2]),
              quants5 = quantile(AAV, probs= perc[1]))
  
  
  strs <- strsplit(files[i], split = '_')[[1]]
  
  runname <- paste(strs[4],strs[6],strsplit(strs[8], split = '.Rdata')[[1]], sep = '_')
  
  catchdf$MP <- runname
  catchcdf$MP <- runname
  catch.tot.tmp$MP <- runname
  
  SSB_mid$MP <- runname
  SSB_tot$MP <- runname
  SSB.tot.tmp$MP <- runname
  SSB.om$MP <- runname
  
  
  AAVdf$MP <- runname
  AAVtottmp$MP <- runname
  AAV.tot.tmp$MP <- runname
  
  HCRname <- strsplit(strs[8], split = '.Rdata')[[1]]
  
  if(HCRname == 'TAC1'){
    HCRname <- as.factor('HCR0')
  }
  if(HCRname == 'TAC2'){
    HCRname <- as.factor('MD')
  }
  if(HCRname == 'TAC3'){
    HCRname <- as.factor('AC')
  }
  
  catchdf$HCR <- HCRname
  catchcdf$HCR <- HCRname
  catch.tot.tmp$HCR <- HCRname
  
  SSB_mid$HCR <- HCRname
  SSB.tot.tmp$HCR <- HCRname
  SSB_tot$HCR <- HCRname
  SSB.om$HCR <- HCRname
  
  AAVdf$HCR <- HCRname
  AAV.tot.tmp$HCR <- HCRname
  AAVtottmp$HCR <- HCRname
  
  catchdf$climate <- strs[6]
  catchcdf$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]
  
  SSB_mid$climate <- strs[6]
  SSB.tot.tmp$climate <- strs[6]
  SSB_tot$climate <- strs[6]
  SSB.om$climate <- strs[6]
  
  AAV.tot.tmp$climate <- strs[6]
  AAVdf$climate <- strs[6]
  AAVtottmp$climate <- strs[6]
  
  if(i == 1){
    catchdfexp <- catchdf
    catchcdfexp <- catchcdf
    catch.tot <- catch.tot.tmp
    
    SSBdf <- SSB_mid
    SSB.tot <- SSB.tot.tmp
    SSB_cdf <- SSB_tot
    ssb.om <- SSB.om
    
    AAVdfexp <- AAVdf
    AAV.tot <- AAV.tot.tmp
    AAVcdf <- AAVtottmp
    
  }else{
    catchdfexp <- rbind(catchdfexp,catchdf)
    catchcdfexp <- rbind(catchcdfexp,catchcdf)
    catch.tot <- rbind(catch.tot,catch.tot.tmp)
    
    SSBdf <- rbind(SSBdf, SSB_mid)
    SSB.tot <- rbind(SSB.tot, SSB.tot.tmp)
    SSB_cdf <- rbind(SSB_cdf, SSB_tot)
    ssb.om <- rbind(ssb.om,SSB.om)
    
    AAVdfexp <- rbind(AAVdfexp, AAVdf)
    AAV.tot <- rbind(AAV.tot, AAV.tot.tmp)
    AAVcdf <- rbind(AAVcdf, AAVtottmp)
    
  }
  
  
  
}


# Table results relative to base - HCR0 

catch.rel <- catch.tot[catch.tot$year > 2030,] %>% 
      mutate(rel = catchmean/catchmean[MP == 'climate_0_TAC1']) 

ggplot(catch.rel, aes(x= year, y=  rel, color = climate))+geom_line()+facet_wrap(~HCR)+theme_bw()+
  scale_y_continuous('catch\nrelative to base scenario')


data %>% 
  group_by(category) %>% 
  mutate(value = value / first(value, order_by = year))