### 
getParms_noage <- function(hakedat){
  
  parms <- hakedat$parameters[is.na(hakedat$parameters$Active_Cnt) == 0,]
  
  nms <- rownames(hakedat$parameters)
  
  psel <- matrix(NA, 5,length(1991:2017))
  psel[1,] <- hakedat$parameters$Value[grep("AgeSel_P3_Fishery\\(1\\)_DEVadd",nms)]
  psel[2,] <- hakedat$parameters$Value[grep("AgeSel_P4_Fishery\\(1\\)_DEVadd",nms)]
  psel[3,] <- hakedat$parameters$Value[grep("AgeSel_P5_Fishery\\(1\\)_DEVadd",nms)]
  psel[4,] <- hakedat$parameters$Value[grep("AgeSel_P6_Fishery\\(1\\)_DEVadd",nms)]
  psel[5,] <- hakedat$parameters$Value[grep("AgeSel_P7_Fishery\\(1\\)_DEVadd",nms)]
  
  
  
  
  Rdev <- hakedat$parameters$Value[43:93] # Maybe find
  
  F0 <-hakedat$derived_quants$Value[grep('F_',rownames(hakedat$derived_quants))]  
  F0 <- F0[1:(length(F0)-3)]#-log(1-F0[1:(length(F0)-3)])
  
  
    
  parms.TMB <- list( # Just start all the simluations with the same initial conditions 
    logRinit = parms$Value[rownames(parms) == 'SR_LN(R0)'],
    logh = log(parms$Value[rownames(parms) == 'SR_BH_steep']),
    logMinit = log(parms$Value[rownames(parms) == 'NatM_p_1_Fem_GP_1']),
    logSDsurv = log(parms$Value[rownames(parms) == 'Q_extraSD_Acoustic_Survey(2)']),
    #logSDR = log(1.4),
    logphi_catch = parms$Value[rownames(parms) == 'ln(EffN_mult)_1'],
    #    logphi_survey = log(11.33),
    # logSDF = log(0.1),
    # Selectivity parameters 
    psel_fish = hakedat$parameters$Value[103:123][3:7],
    psel_surv = hakedat$parameters$Value[124:143][4:7],
    initN = rev(hakedat$parameters$Value[23:42]),
    Rin = Rdev,
    F0 = F0,
    PSEL = psel
  )
  
return(parms.TMB)  
}