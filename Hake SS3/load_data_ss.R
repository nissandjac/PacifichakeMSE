## Load the hake data
# year and age input 
load_data_ss <- function(mod,
                         sum_zero = 0){
  
  #' @mod SS3 file 
  #' @sum_zero flag on whether 'main' recruitment deviations should sum to zero
  
  years <- mod$startyr:mod$endyr
  tEnd <- length(years)
  age <- 0:mod$accuage
  
  F0 <- mod$catch$F[mod$catch$Yr >=years[1] & mod$catch$Yr <(max(years)+1)]
  
  nage <- length(age)
  msel <- rep(1,nage)
  # Maturity
  mat <- as.numeric(mod$ageselex[mod$ageselex$Factor == 'Fecund' & mod$ageselex$Yr == 1963,paste(age)])
  # weight at age 
  wage_ss <- mod$wtatage

  wage_ssb <- wage_ss[wage_ss$Fleet == -2,paste(age)]
  wage_catch <- wage_ss[wage_ss$Fleet == 1 & wage_ss$Yr > (years[1]-1) & wage_ss$Yr < years[tEnd]+1, paste(age)]
  wage_survey <- wage_ss[wage_ss$Fleet == 2 & wage_ss$Yr > (years[1]-1) & wage_ss$Yr < years[tEnd]+1, paste(age)]
  wage_mid <- wage_ss[wage_ss$Fleet == -1 & wage_ss$Yr > (years[1]-1) & wage_ss$Yr < years[tEnd]+1, paste(age)]
  
  
  catch <- mod$catch$ret_bio[mod$catch$Yr> (years[1]-1) & (mod$catch$Yr < years[tEnd]+1)]
  # Survey abundance
  survey <- rep(1, tEnd)
  
  sidx <- mod$cpue$Yr[mod$cpue$Use == 1]
  survey[which(years %in% sidx)] <- mod$cpue$Obs[mod$cpue$Use == 1]
  ss.error <- rep(1,tEnd)
  ss.error[which(years %in% sidx)]  <- (1+mod$cpue$SE[mod$cpue$Use == 1])-mod$cpue$SE[2] # Dunno why this calc is necessary 
  
  
  # Age comps
  
  survey.ss <- mod$agedbase[mod$agedbase$Fleet == 2,]
  
  age_survey_ss <- data.frame(year = survey.ss$Yr, age = survey.ss$Bin, obs = survey.ss$Obs,
                              N = survey.ss$N)
  
  age_survey.tmp <- matrix(-1,length(mod$agebins), tEnd)
  
  syears <- unique(survey.ss$Yr)
  
  survey_x <- rep(-2, tEnd) # 
  survey_flag <- rep(-1, tEnd) 
  
  ss.survey <- rep(0, tEnd)
  
  for(i in 1:tEnd){

    if(years[i] %in% syears){
      
    tmp <- age_survey_ss[age_survey_ss$year == years[i],]  
    age_survey.tmp[,i] <- tmp$obs
    ss.survey[i] <- mean(tmp$N)
    survey_x[i]<- 2
    survey_flag[i] <- 1
    }else{
    age_survey.tmp[,i] <- -1  
    }
  }

  ###### Order the catch age comps 
  
  catch.ss <- mod$agedbase[mod$agedbase$Fleet == 1,]
  
  age_catch_ss <- data.frame(year = catch.ss$Yr, age = catch.ss$Bin, obs = catch.ss$Obs,
                              N = catch.ss$N)
  
  age_catch.tmp <- matrix(-1,length(mod$agebins), tEnd)
  
  cyears <- unique(catch.ss$Yr)
  cflag <- rep(-1, tEnd)
  ss.catch <- rep(0, tEnd)
  
  for(i in 1:tEnd){
    
    if(years[i] %in% cyears){
      
      tmp <- age_catch_ss[age_catch_ss$year == years[i],]  
      age_catch.tmp[,i] <- tmp$obs
      ss.catch[i] <- mean(tmp$N)
      cflag[i] <- 1
    }else{
      age_catch.tmp[,i] <- -1  
    }
  }
  

  b <- matrix(NA, tEnd)
  # Parameters
  yb_1 <- as.numeric(mod$breakpoints_for_bias_adjustment_ramp[1]) #_last_early_yr_nobias_adj_in_MPD
  yb_2 <- as.numeric(mod$breakpoints_for_bias_adjustment_ramp[2]) #_first_yr_fullbias_adj_in_MPD
  yb_3 <- as.numeric(mod$breakpoints_for_bias_adjustment_ramp[3]) #_last_yr_fullbias_adj_in_MPD
  yb_4 <- as.numeric(mod$breakpoints_for_bias_adjustment_ramp[4]) #_first_recent_yr_nobias_adj_in_MPD
  b_max <- as.numeric(mod$breakpoints_for_bias_adjustment_ramp[5]) #_max_bias_adj_in_MPD

  #b[1] <- 0
  for(j in 1:length(years)){

    if (years[j] <= yb_1){
      b[j] = 0}

    if(years[j] > yb_1 & years[j]< yb_2){
      b[j] = b_max*((years[j]-yb_1)/(yb_2-yb_1));
    }

    if(years[j] >= yb_2 & years[j] <= yb_3){
      b[j] = b_max}

    if(years[j] > yb_3 & years[j] < yb_4){
      b[j] = b_max*(1-(yb_3-years[j])/(yb_4-yb_3))
    }

    if(years[j] >= yb_4){
      b[j] = 0
    }

  }
  
  ### h prior distribution ###
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
  
  ### selyear
  sel.tmp <- mod$SelAgeAdj$Yr[mod$SelAgeAdj$`Change?` == 1 & mod$SelAgeAdj$Yr>years[1]][1]
  flag_sel <- rep(0,length(years))
  flag_sel[years %in% unique(mod$SelAgeAdj$Yr[mod$SelAgeAdj$`Change?` == 1 & mod$SelAgeAdj$Yr>years[1]])] <- 1
  
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
                  year_sel = length(sel.tmp:years[length(years)]), # Years to model time varying sel
                  selYear = which(sel.tmp == years),
                  flag_sel = flag_sel,
                  tEnd = length(years), # The extra year is to initialize 
                  logQ = mod$parameters$Value[mod$parameters$Label == "LnQ_base_Acoustic_Survey(2)"],   # Analytical solution
                  # Selectivity 
                  Smin = 1,
                  Smin_survey = 2,
                  Smax = 6,
                  Smax_survey = 6,
                  # survey
                  survey = survey, # Make sure the survey has the same length as the catch time series
                  survey_err = ss.error, # Make sure the survey has the same length as the catch time series
                  survey_x = survey_x, # Is there a survey in that year?
                  ss_survey = ss.survey, # sample sizes of ages
                  flag_survey =survey_flag, # Are there age comps that year
                  age_survey = age_survey.tmp,
                  age_maxage = max(mod$agebins), # Max age for age comps 
                  # Catch
                  Catchobs = catch, # Convert to kg
                  ss_catch = ss.catch, # Age comp sample sizes
                  flag_catch =cflag, # Are the age comps from the catch? 
                  age_catch = age_catch.tmp,
                  # variance parameters
                  logSDcatch = log(mod$catch$se[1]),
                  logSDR = log(mod$parameters$Value[mod$parameters$Label == 'SR_sigmaR']), # Fixed in stock assessment ,
                  F0 = F0,
                  #logphi_survey = log(0.91),
                  sigma_psel = mod$parameters$Value[mod$parameters$Label == "AgeSel_P3_Fishery(1)_dev_se"],
                  smul = 0.5,
                  sum_zero = sum_zero,
                  years = years,
                  logphi_survey = mod$parameters$Value[mod$parameters$Label == "ln(EffN_mult)_2"],
                  Bprior= tau*mu,
                  Aprior = tau*(1-mu),
                  b = b#,
                  #    ageerr = as.matrix(age_err[,2:22])
  )
  
  
  return(df)
  
}




