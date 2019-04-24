#### commands to load model output
if(FALSE){
  devtools::install_github('r4ss/r4ss')
  require(r4ss)
  mydir <- "~/GitHub/PacifichakeMSE/Hake SS3 version"
  hake2018b <- SS_output(mydir)
}

#### get spawning biomass
# note that 1966 is the start of the time series the value associated with
# 1964 is B0 and the 1965 value is an initial fished equilibrium
# which in the hake model is not used so is identical to B0

# subset columns of timeseries
SpawnBio <- hake2018b$timeseries[,c("Yr","SpawnBio","Era")]
# divide by 2
SpawnBio$SpawnBio <- SpawnBio$SpawnBio/2
head(SpawnBio)
##        Yr SpawnBio  Era
## 1044 1964  2579870 VIRG
## 1045 1965  2579870 INIT
## 1046 1966   739615 TIME
## 1047 1967   715200 TIME
## 1048 1968   705775 TIME
## 1049 1969   797835 TIME

#### get parameter estimates
# (those with NA for Active_Cnt are not estimated)
pars <- hake2018b$parameters[,c("Value","Active_Cnt")]
head(pars)
##                        Value Active_Cnt
## NatM_p_1_Fem_GP_1   0.216095          1
## L_at_Amin_Fem_GP_1  5.000000         NA
## L_at_Amax_Fem_GP_1 53.200000         NA
## VonBert_K_Fem_GP_1  0.300000         NA
## CV_young_Fem_GP_1   0.066000         NA
## CV_old_Fem_GP_1     0.062000         NA

#### get likelihoods
# first by type (summed across fleets)
(likelihoods <- hake2018b$likelihoods_used)
##                            values lambdas
## TOTAL                 5.12392e+02      NA
## Catch                 3.32171e-13      NA
## Equil_catch           0.00000e+00       1
## Survey               -6.72696e+00      NA
## Age_comp              4.19985e+02      NA
## Recruitment           3.51427e+01       1
## Forecast_Recruitment  0.00000e+00       1
## Parm_priors           4.18123e-01       1
## Parm_devs             6.35730e+01       1
## Crash_Pen             0.00000e+00       1

# likelihoods by fleet (only needed to see breakdown of age comps
# into 339.8290 for fishery and 80.15620 for the survey)
(hake2018b$likelihoods_by_fleet)
##            Label          ALL     Fishery Acoustic_Survey
## 100 Catch_lambda           NA 1.00000e+00         1.00000
## 101   Catch_like  3.32085e-13 3.32085e-13         0.00000
## 102  Surv_lambda           NA 0.00000e+00         1.00000
## 103    Surv_like -6.72693e+00 0.00000e+00        -6.72693
## 104   Age_lambda           NA 1.00000e+00         1.00000
## 105     Age_like  4.19985e+02 3.39829e+02        80.15570

if(FALSE){
  save(hake2018b, SpawnBio, pars, likelihoods,
       file=file.path(mydir, 'hake2018_no_ageerror_fixtheta.RData'))
}
