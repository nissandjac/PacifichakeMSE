getParameters_ss <- function(trueparms = TRUE, mod = NA){
  
  
  
  if (trueparms == TRUE){
     
     if(all(is.na(mod))){
        stop('require SS3 model as input')
      }
          # (those with NA for Active_Cnt are not estimated)
      pars <- mod$parameters[,c("Value","Active_Cnt")]
      pars <- pars[is.na(pars$Active_Cnt) == 0,] # Remove parameters that are not estimated
      nms <- rownames(pars)
      
      ### Get the Rdev params
      ninit.idx <- grep('Early_Init', nms)
      
      sel.tmp <- mod$SelAgeAdj$Yr[mod$SelAgeAdj$`Change?` == 1 & mod$SelAgeAdj$Yr>years[1]][1]
      
     
      ridx <- c(grep('Early_RecrD', nms), grep('Main_Recr',nms))
      pselidx <- grep('Fishery',nms)
      pselidx <- pselidx[-grep('DEVadd', nms[pselidx])]
      
      surv.idx <- grep('Acoustic_Survey',nms)
      surv.idx <- surv.idx[-grep('Q_ex', nms[surv.idx])]
      
      
      PSEL <- matrix(0,length(pselidx), length(sel.tmp:df$years[length(df$years)]))
      
      fish_ages <- c('P3','P4', 'P5', 'P6', 'P7')
      
      for(i in 1:length(fish_ages)){ # Generalize later
        idx <- grep(paste('AgeSel_',fish_ages[i],'_F',sep = ''), nms)
        
        nms[idx[2:length(idx)]] # Something wrong with the naming corrections. 
        PSEL[i,] <- pars$Value[idx[2:length(idx)]]
      }
      colnames(PSEL) <- 1991:max(df$years)
      
      parms <- list( # Just start all the simluations with the same initial conditions 
        logRinit = pars$Value[which(nms == "SR_LN(R0)")],
        logh = log(pars$Value[which(nms == "SR_BH_steep")]),
        logMinit = log(pars$Value[which(nms == "NatM_p_1_Fem_GP_1")]),
        logSDsurv = log(pars$Value[which(nms == "Q_extraSD_Acoustic_Survey(2)")]),
        #logSDR = log(1.4),
        logphi_catch = pars$Value[which(nms =="ln(EffN_mult)_1")],
        # logphi_survey = log(10),
        # logSDF = log(0.1),
        # Selectivity parameters 
        psel_fish = pars$Value[pselidx],
        psel_surv = pars$Value[surv.idx],
        initN = rev(pars$Value[ninit.idx]),
        Rin = pars$Value[ridx], # Perhaps should be 75 - check
        # F0 = mod$derived_quants$Value[grep('F_',rownames(mod$derived_quants))][1:df$tEnd],
        PSEL = PSEL,
        F0 = mod$catch$F[2:length(mod$catch$F)]
        
      )
      
    
    
  }else{
    
    PSEL <- matrix(0,5, length(1991:years[length(years)]))
    initN <- rep(0,df$nage-1)
    F0 <- rep(0.01, df$tEnd)
    Rdev <- rep(0, df$tEnd-1)
    #Rdev <- read.csv('Rdev_MLE.csv')[,1]
    
    
    parms <- list( # Just start all the simluations with the same initial conditions 
      logRinit = 16,
      logh = log(0.7),
      logMinit = log(0.3),
      logSDsurv = log(0.3),
      # logSDR = log(1.4),
      logphi_catch = log(0.8276),
      # logphi_survey = log(11.33),
      # logSDF = log(0.1),
      # Selectivity parameters 
      psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
      psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
      initN = initN,
      Rin = Rdev,
      F0 = F0,
      PSEL = PSEL
    )
    
    
  }
  return(parms) 
}