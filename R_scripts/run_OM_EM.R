#### Run a simulation run of the OM and  EM ####

require(TMB)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)
require(scales)
require(RColorBrewer)
library(r4ss)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
source('load_files.R')
source('load_files_OM.R')
source('fnMSE.R')
source('hake_objectives.R')
set.seed(12345)
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)

SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]
R.ss <- mod$derived_quants$Value[grep('Recr_1966', mod$derived_quants$Label):grep('Recr_2018', mod$derived_quants$Label)]

###### Load the data to run the MSE ######
df <- load_data_seasons(nseason = 1, nspace = 1,
                        nsurvey= 2, movemax = 0.4) # Prepare data for operating model 
#df$surveyseason <- 2

simyears <- 50
TAC <- 1
nruns <- 1
sim.data <- run.agebased.true.catch(df)


# Plott stuff 

parms <- getParameters_OM(trueparms = TRUE, df = df)

##  Create a data frame to send to runHakeassessment 

df.new <- create_TMB_data(sim.data, df, history = TRUE)

parms.new <- parms
F0 <- rowSums(sim.data$Fout)
Rdev <- parms$Rin
parms.new$F0 <- F0
parms.new$Rin <- Rdev

obj <-MakeADFun(df.new,parms.new,DLL="runHakeassessment") # Run the assessment 

reps <- obj$report()

# SSB 
plot(df$years,rowSums(sim.data$SSB.weight))
lines(df$years,SSB.ss3*0.5)
lines(df$years,reps$SSB, col = 'red')
# Survey 
plot(df$years,sim.data$survey.true, type ='l')
points(df$years[df$survey_x == 2],df$survey[df$survey_x == 2,])
points(df$years[df$survey_x == 2],df.new$survey[df$survey_x == 2], col = 'red')
points(df$years[df$survey_x == 2],reps$Surveyobs[df$survey_x == 2], col = 'green')


lower <- obj$par-Inf
lower[names(lower) == 'F0'] <- 0.001
upper <- obj$par+Inf
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logphi_catch'] <- log(15)
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2
upper[names(upper) == 'psel_fish'] <- 3
lower[names(lower) == 'psel_fish'] <- 0.0001


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e6, 
                                       eval.max = 1e6))) 

rep <- sdreport(obj)
sdrep <- summary(rep)
rep.values<-rownames(sdrep)
nyear <- df$tEnd
  
R <- data.frame(name = sdrep[rep.values == 'R',1])
  
SSB <- data.frame(name = sdrep[rep.values == 'SSB',1])
SSB$SE <- sdrep[rep.values == 'SSB',2]
SSB$min <- SSB$name-2*SSB$SE
SSB$max <- SSB$name+2*SSB$SE
SSB$year <- df$years

Catch <- getUncertainty('Catch', df.new, sdrep)
SSB <- getUncertainty('SSB',df.new, sdrep)

plot(SSB$value)
lines(rowSums(sim.data$SSB))
lines(SSB.ss3*0.5, col = 'red')


# Compare the estimated parameters 
df.p <- as.data.frame(rep$par.fixed)
df.p$name <- names(rep$par.fixed)
df.p$idx <- 1:nrow(df.p)
df.p$model <- 'TMB'
names(df.p)[1] <- 'parameter'
df.p2 <- as.data.frame(unlist(parms.new))
df.p2$name <- names(rep$par.fixed)

df.p2$idx <- 1:nrow(df.p2)
names(df.p2)[1] <- 'parameter'
df.p2$model <- 'SS3'
df.plot <- rbind(df.p,df.p2)

# Fix the log values 
idx <- grep('log', df.plot$name)
df.plot$parameter[idx] <- exp(df.plot$parameter[idx])

ggplot(df.plot, aes(x=  idx, y = parameter, color = model))+geom_point()+
  facet_wrap(~name,scales = 'free')+theme_classic()


ss.exp <- rep(NA, df$nyear)
SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]


df.ss <- data.frame(year = rep(df$years,3), 
                    SSB = c(SSB$value,
                            reps$SSB,
                            SSB.ss3*0.5
                    ),
                    model = rep(c('TMB est','TMB SS3 parms','SS3'), each = df$tEnd)
)

cols <- PNWColors::pnw_palette('Starfish', n = length(unique(df.ss$model)))

p.ssb <- ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = SSB*1e-6, color = model))+
  geom_line()+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),])+
  theme_classic()+scale_y_continuous('Spawning biomass\n (million tonnes)')+
  scale_color_manual(values = cols)+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())+
  geom_ribbon(data = SSB, aes(ymin = min*1e-6, ymax = max*1e-6, x = year,
                              y= value*1e-6, color = NA, group = NA),
              fill = alpha('gray', alpha = 0.3), color = NA)

p.ssb
