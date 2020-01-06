## Load the hake data
# year and age input 
load_data <- function(){


years <- 1966:2017 # Manuually change the years
tEnd <- length(years)
age <- 0:20

# F0 <- assessment$F0
# F0 <- mod$derived_quants$Value[grep('F_1966',mod$derived_quants$Label):grep('F_2017',mod$derived_quants$Label)]
#F <- mod$catch$F[mod$catch$Yr >1965 & mod$catch$Yr <(max(years)+1)]

nage <- length(age)
msel <- rep(1,nage)
# Maturity
#mat <- as.numeric(mod$ageselex[mod$ageselex$Factor == 'Fecund' & mod$ageselex$Yr == 1963,paste(age)])3
mat <- read.csv('data/maturity.csv')
mat <- mat$mat
# weight at age 
#wage_ss <- mod$wtatage
wage <- read.csv('data/waa.csv')
wage_unfished <- read.csv('data/unfished_waa.csv')

# Make the weight at ages the same length as the time series 
wage_ssb = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                        as.matrix(wage[wage$fleet == 0,3:(nage+2)]))

wage_survey = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                    as.matrix(wage[wage$fleet == 2,3:(nage+2)]))

wage_catch = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                   as.matrix(wage[wage$fleet == 1,3:(nage+2)]))

wage_mid = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                as.matrix(wage[wage$fleet == -1,3:(nage+2)]))

# Catch
# catch <- read.csv('hake_totcatch.csv')
catches.obs <- read.csv('data/catches.csv')
catch <- catches.obs$Total

# Survey abundance
df.survey <- read.csv('data/acoustic survey.csv')



# Age comps

age_survey.df <- read.csv('data/agecomps_survey.csv')
age_survey.df$flag <- 1
age_catch.df <- read.csv('data/agecomps_fishery.csv')
age_catch.df$flag <- 1
# Insert dummy years

age_survey <- as.data.frame(matrix(-1, tEnd,dim(age_survey.df)[2]))
names(age_survey) <- names(age_survey.df)
age_survey$year <- years
age_catch <- as.data.frame(matrix(-1, tEnd,dim(age_catch.df)[2]))
names(age_catch) <- names(age_catch.df)
age_catch$year <- years

for (i in 1:dim(age_survey.df)[1]){
    idx <- which(age_survey$year == age_survey.df$year[i])
    age_survey[idx,] <-age_survey.df[i,]
  
}

for (i in 1:dim(age_catch.df)[1]){
  idx <- which(age_catch$year == age_catch.df$year[i])
  age_catch[idx,] <-age_catch.df[i,]
  
}

# Calculate the bias adjustment 
b <- matrix(NA, tEnd)
Yr <- 1946:2017
# Parameters 
yb_1 <- 1965 #_last_early_yr_nobias_adj_in_MPD
yb_2 <- 1971 #_first_yr_fullbias_adj_in_MPD
yb_3 <- 2016 #_last_yr_fullbias_adj_in_MPD
yb_4 <- 2017 #_first_recent_yr_nobias_adj_in_MPD
b_max <- 0.87 #_max_bias_adj_in_MPD

b[1] <- 0
for(j in 2:length(Yr)){
  
  if (Yr[j] <= yb_1){
    b[j] = 0}
  
  if(Yr[j] > yb_1 & Yr[j]< yb_2){
    b[j] = b_max*((Yr[j]-yb_1)/(yb_2-yb_1));
  }
  
  if(Yr[j] >= yb_2 & Yr[j] <= yb_3){
    b[j] = b_max}
  
  if(Yr[j] > yb_3 & Yr[j] < yb_4){
    b[j] = b_max*(1-(yb_3-Yr[j])/(yb_4-yb_3))
  }
  
  if(Yr[j] >= yb_4){
    b[j] = 0
  }
  # if (b[j]<b[j-1]){
  #   stop('why')
  # }
}  

### h prior distribution
hmin <- 0.2
hmax <- 1
hprior <- 0.777
hsd <- 0.117

mu <- (hprior-hmin)/(hmax-hmin)
tau <- ((hprior-hmin)*(hmax-hprior))/hsd^2-1


Bprior= tau*mu
Aprior = tau*(1-mu)
Pconst <- 1e-6
hrange <- seq(0.2,1, length.out = 100)

Prior_Like =  (1.0-Bprior)*log(Pconst+hrange-hmin) + 
              (1.0-Aprior)*log(Pconst+hmax-hrange)-
              (1.0-Bprior)*log(Pconst+hprior-hmin) - 
              (1.0-Aprior)*log(Pconst+hmax-hprior)


## Load the ageing error 
# age_err <- read.csv('data/age_error_head.csv')
# names(age_err)[1] <- 'age'
# age_err <- apply(age_err, 2, rev)
# 
# # fix the input matrices to  add ageing error 
# ## survey 
# survey_age <- matrix(-1, tEnd,nage-1)
# 
# for(i in 1:tEnd){
#   if(age_survey$flag[i] == 1){
#     for(j in 1:20){
#       survey_age[i,j] <- age_survey[i,j+2]
#       
#     }  
# 
#   }
#   
#   
# }

df <-list(      #### Parameters #####
                wage_ssb = t(wage_ssb),
                wage_catch = t(wage_catch),
                wage_survey = t(wage_survey),
                wage_mid = t(wage_mid),
                #  Input parameters
                Msel = msel,
                Matsel= mat,
                nage = nage,
                age = age,
                year_sel = length(1991:years[length(years)]), # Years to model time varying sel
                selYear = 26,
                tEnd = length(years), # The extra year is to initialize 
                logQ = log(1.135767),   # Analytical solution
                # Selectivity 
                Smin = 1,
                Smin_survey = 2,
                Smax = 6,
                Smax_survey = 6,
                # survey
                survey = c(rep(1,df.survey$Year[1]-years[1]),df.survey$obs), # Make sure the survey has the same length as the catch time series
                survey_err = c(rep(1,df.survey$Year[1]-years[1]),df.survey$se.log.), # Make sure the survey has the same length as the catch time series
                survey_x = c(rep(-2,df.survey$Year[1]-years[1]),df.survey$fleet), # Is there a survey in that year?
                ss_survey = age_survey$nTrips,
                flag_survey =age_survey$flag,
                age_survey = t(as.matrix(age_survey[,3:17])*0.01),
                age_maxage = 15, # Max age for age comps 
                logSDF = log(0.1),
                # Catch
                Catchobs = catch, # Convert to kg
                ss_catch = age_catch$nTrips,
                flag_catch =age_catch$flag,
                age_catch = t(as.matrix(age_catch[,3:17])*0.01),
                # variance parameters
                logSDcatch = log(0.01),
                logSDR = log(1.4), # Fixed in stock assessment ,
               # F0 = F0,
                #logphi_survey = log(0.91),
                sigma_psel = 1.4,
                smul = 1,
                logh = log(0.8),
                years = years,
                logphi_catch = log(0.8276), # log(0.8276)
                logphi_survey = log(10),
                Bprior= tau*mu,
                Aprior = tau*(1-mu),
                sum_zero = 0,
                b = b[Yr >= years[1]]#,
            #    ageerr = as.matrix(age_err[,2:22])
)


return(df)

}




