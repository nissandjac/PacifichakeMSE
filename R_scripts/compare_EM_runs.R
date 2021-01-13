## Compare perfect and EM-OM runs

library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)
library(purrr)
library(dplyr)
library(reshape2)
library(patchwork)
load_all()
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
EM.folder <- 'C:/Users/Nis/Dropbox/NOAA/Hake MSE/MSE results/final results/'
#
p.folder <- 'C:/Users/Nis/Dropbox/NOAA/Hake MSE/MSE results/perfect/'
# #
p0 <- 'C:/Users/Nis/Dropbox/NOAA/Hake MSE/MSE results/MSErun_move_JMC_climate_0_04_HYBR_TAC3_perfect_nofishing.Rdata'

EM.files <- dir(EM.folder)[grep(dir(EM.folder),pattern = '.Rdata')]
p.files <- dir(p.folder)[grep(dir(p.folder),pattern = '.Rdata')]
# Recreate the violin pltos

yrs <- 1966:2047

perc <- c(0.05,0.9) # Percentiles for plotting


# Just look at a couple of individual runs

for(i in 1:length(p.files)){

  load(paste(EM.folder,EM.files[i], sep = '')) # Prints as 'ls.save'
  df.MSE.EM <- flatten(ls.save) # Change the list a little bit

  load(paste(p.folder,p.files[i], sep = '')) # Prints as 'ls.save'
  df.MSE.p <- flatten(ls.save) # Change the list a little bit



  SSB_EM <- processMSE(df.MSE.EM, id = 'SSB', idx = 1, spacenames = c('value'), runs = 100,nspace = 2)

  # Remove runs 101:500 for now
  SSB_EM <- filter(SSB_EM, run %in% paste('run',1:100, sep = ''))


  SSB_p <- processMSE(df.MSE.p, id = 'SSB', idx = 1, spacenames = c('value'), runs = 1000,nspace = 2)

  SSB <- bind_rows(list(EM = SSB_EM,perfect = SSB_p), .id = "id")

  catch_EM <- processMSE(df.MSE.EM, 'Catch', idx = 2, spacenames = c('value'), runs = 500,nspace = 2)
  catch_EM <- filter(catch_EM, run %in% paste('run',1:100, sep = ''))

  catch_p <- processMSE(df.MSE.p, 'Catch', idx = 2, spacenames = c('value'), runs = 1000,nspace = 2)

  catch <- bind_rows(list(EM = catch_EM, perfect = catch_p), .id = 'id')


  catch.tot.tmp <- catch %>%
    group_by(year, id) %>%
    summarise(catchmean = median(value),
              quants95 = quantile(value, probs =perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  SSB_tot<- SSB  %>%
    group_by(year,id) %>%
    summarise(SSBmean = median(value),
              quants95 = quantile(value, probs = perc[2]),
              quants5 = quantile(value, probs= perc[1]))

  strs <- strsplit(EM.files[i], split = '_')[[1]]

  runname <- paste(strs[4],strs[6],strsplit(strs[8], split = '.Rdata')[[1]], sep = '_')

  catch.tot.tmp$MP <- runname
  SSB_tot$MP <- runname
  SSB$MP <- runname
  # HCR
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


  catch.tot.tmp$HCR <- HCRname
  SSB_tot$HCR <- HCRname
  SSB$HCR <- HCRname

  catch.tot.tmp$climate <- strs[6]
  SSB_tot$climate <- strs[6]
  SSB$climate <- strs[6]


  if(i == 1){
    catch.tot <- catch.tot.tmp
    SSB.tot <- SSB_tot
    SSB_cdf <- SSB
  }else{

    catch.tot <- rbind(catch.tot,catch.tot.tmp)

    SSB_cdf <- rbind(SSB_cdf, SSB)
    SSB.tot <- rbind(SSB.tot, SSB_tot)
  }

}


# Calculate risk
risk <- SSB_cdf %>% mutate(rel = value/sum(sim.data$SSB_0))

risk.year <- risk %>% group_by(year, MP, HCR, climate,id) %>%
  summarise(riskmed = median(rel),
            quants95 = quantile(rel, probs =0.99),
            quants5 = quantile(rel, probs= 0.01)
  )



risk.sum <- risk[risk$year>2018,]
risk.sum$closed <- 1
risk.sum$closed[which(risk.sum$rel < 0.1)] <- 0

risk.sum$decline <- 1
risk.sum$decline[which(risk.sum$rel < 0.4)] <- 0


# Risk per metric long term
risk.time <- risk.sum %>%
  group_by(MP, HCR, climate, year,id) %>%
  summarise(risk = length(which(rel< 0.1))/length(which(rel > 0.1)))

cols <- PNWColors::pnw_palette('Starfish',n = 3, type = 'discrete')

lsize <- 0.5
usize <-  0.3
pyear <- 2018

cplot <- ggplot(catch.tot[catch.tot$year>pyear,], aes(x = year, y = catchmean*1e-6, color = climate, linetype = id))+
  geom_line(size = lsize)+theme_bw()+facet_grid(climate~HCR)+
  theme(legend.position = 'top',
        text = element_text(size = 8),
        legend.title = element_blank(),
        strip.text = element_text(size=5),
        axis.text.x = element_text(size = 5, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        legend.margin = margin(c(0,0,0,0)))+
  scale_color_manual(values = cols, labels = c('baseline', 'moderate', 'high'))+
  # geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+
  # geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+
  scale_y_continuous('Catch/\n(million tonnes)')+
  scale_x_continuous('')+
  geom_line(data = catch.tot[catch.tot$year< 2019 & catch.tot$year> pyear,] ,aes(x = year, y= catchmean*1e-6), color = 'black', size = lsize)
cplot

# ssbplot <- ggplot(SSB.tot[SSB.tot$year>pyear,],
#                   aes(x = year, y = SSBmean*1e-6, color = climate, linetype = id))+geom_line(size = lsize)+theme_bw()+
#   theme(legend.position = 'none',
#         text = element_text(size = 8),
#         strip.text = element_text(size=5),
#         axis.text.x = element_text(size = 5, angle = 90),
#         plot.margin = unit(c(1,1,0,1), 'pt'))+
#   # geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+
#   # geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+
#   scale_color_manual(values = cols)+
#   scale_x_continuous('')+facet_wrap(~HCR, ncol = 3)+scale_y_continuous('SSB')+
#   geom_line(data = SSB.tot[SSB.tot$year< 2019 & SSB.tot$year>pyear,] ,aes(x = year, y= SSBmean*1e-6), color = 'black', size = lsize)

riskplot <- ggplot(risk.year[risk.year$year>pyear,],
                   aes(x = year, y = riskmed, color = climate, linetype = id))+geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        strip.text = element_text(size=5),
        axis.text.x = element_text(size = 5, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  geom_line(aes(y = quants5), size = usize)+
  geom_line(aes(y = quants95), linetype = 2, size = usize)+
  scale_color_manual(values = cols)+coord_cartesian(ylim = c(0,0.7))+
  geom_hline(aes(yintercept = .1), linetype = 2, color = 'black')+
  scale_x_continuous('')+
  facet_grid(climate~HCR)+scale_y_continuous('SSB/\nSSB0')+
  geom_line(data = risk.year[risk.year$year< 2019 & risk.year$year>pyear,] ,aes(x = year, y= riskmed), color = 'black', size = lsize)

riskplot



# Risk plots

riskrun <- risk[risk$year>2020,] %>%
  group_by(run, MP, HCR, climate, id) %>%
  summarise(nyears = n(),
            nover = sum(rel>0.1),
            frac = nover/nyears)

risklines <- ggplot(risk.time, aes(x = year, y = risk, color = climate, linetype = id))+
  geom_line()+
  facet_grid(climate~HCR)+
  theme_classic()+geom_hline(aes(yintercept = 0.05), linetype = 2)+
  scale_color_manual(values = cols[1:3], labels = c("baseline", "moderate", "high"))+
  coord_cartesian(ylim = c(0,0.2)) +
  theme(strip.background =element_rect(fill="white"))+theme(legend.position = 'top',
                                                            legend.title = element_blank(),
                                                            legend.background = element_blank())


risklines

png('results/Climate/risklines_perfect.png', width = 25, height =12, res = 400, unit = 'cm')
risklines
dev.off()


png('results/Climate/objectives_publication_perfect.png', width = 32, height =28, res = 400, unit = 'cm')

cplot/riskplot

dev.off()




# Look at the risk from some of the individual runs


xx <- risk.sum %>%
  group_by(id, run, MP, HCR, climate) %>%
  summarise(nclosed = sum(closed))

idx <- unique(xx[order(xx$nclosed),]$run[1:30])



ggplot(risk.sum[risk.sum$run %in% idx & risk.sum$id == 'perfect',],
       aes(x = year, y = rel, color = HCR, group = run))+
  geom_line()+
  facet_grid(HCR~climate)+
  theme_bw()+coord_cartesian(ylim = c(0,2))+geom_hline(aes(yintercept = 0.1))



# Look at the crashed years in perfect info

tt <- filter(risk.sum, id == 'EM' & closed == 0)


p1 <- ggplot(filter(risk.sum,run == 'run5' & id == 'perfect'), aes(x = year, y= rel, color = HCR))+geom_line()+theme_bw()+facet_wrap(~climate)

p2 <- ca





