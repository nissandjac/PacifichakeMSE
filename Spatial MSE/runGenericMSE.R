##### Run the hake MSE with different options 
# Load required packages and files 
require(TMB)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)
require(scales)
require(RColorBrewer)



compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
source('load_files.R')
source('fnMSE.R')


###### Load the data to run the MSE ######
parms <- load_data_seasons(move = TRUE, nseason = 4, nspace = 2,
                        nsurvey= 2) # Prepare data for operating model 


###### Run the MSE using the data above #####
ls <- fnMSE(parms, simyears = 50, TAC = 1, nruns = 10) # TAC 1) HCR, 2) JTC, 3) Realized catch
MSE <- ls[[1]]
sim.data <- ls[[2]]

objectives <- hake_objectives(MSE,sim.data,simyears = 50 ,move = 1)
objectives[[2]]

###### Plot some results ######

df.plot <- df_lists(MSE, 'HCR', simyears = 50)
names(df.plot[[3]])

cols <- brewer.pal(6, 'Dark2')


p2.1 <- ggplot(df.plot[[3]]$Catchplot, aes(x = year, y = med*1e-6))+geom_line(size = 2)+
  geom_ribbon(aes(ymin = p5*1e-6, ymax = p95*1e-6), linetype = 2, fill = alpha(cols[1], alpha = 0.2))+
  scale_color_manual(values=cols[1:3])+scale_y_continuous(name = 'Catch (million tonnes)')+
  geom_line(aes(y = p5*1e-6, color = run), linetype = 2)+geom_line(aes(y = p95*1e-6, color = run), linetype = 2)+
  facet_wrap(~run)

p2.1
