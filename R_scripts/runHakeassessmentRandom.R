# Run the hake assessment
library(r4ss)
library(PacifichakeMSE)
# Read the assessment data
devtools::load_all()

mod <- SS_output(paste(getwd(),'/inst/extdata/SS32019', sep =''), printstats=FALSE, verbose = FALSE)

# SSB from the assessment
SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2019', mod$derived_quants$Label)]

parms <- getParameters_ss(mod)
df <- load_data_ss(mod)


compile("src/runHakeassessment.cpp")
dyn.load(dynlib("src/runHakeassessment"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment", random = c('Rin','initN','PSEL','F0')) # Takes forever with random effects - model is not well defined

vars <- obj$report()

lower <- obj$par-Inf

lower[names(lower) == 'F0'] <- 0.001
upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e5,
                                       eval.max = 1e5))
            ) #

system.time(rep<-sdreport(obj))
rep

sdrep <- summary(rep)
rep.values<-rownames(sdrep)


SSB <- getUncertainty('SSB',df, sdrep)
F0 <- getUncertainty('Fyear',df, sdrep)
Catch <- getUncertainty('Catch',df, sdrep)
Surveyobs <- getUncertainty('Surveyobs',df, sdrep)
R <- getUncertainty('R',df, sdrep)
surveyselec.est <- getUncertainty('surveyselc', df, sdrep)
catchselec.est <- getUncertainty('catchselec', df, sdrep)
SSB0 <- getUncertainty('SSBzero', df, sdrep)


SSB$value <- SSB$value*1e-6
SSB$min <- SSB$min*1e-6
SSB$max <- SSB$max*1e-6

plotValues(SSB, data.frame(x= df$years, y= SSB.ss3*0.5*1e-6),'SSB')

# Estimated SDR is 0.75

exp(rep$par.fixed[5])
