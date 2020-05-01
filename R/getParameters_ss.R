#' Load parameters from stock synthesis
#'
#' @param trueparms Should the parameters be the
#' @param mod
#'
#' @return
#' returns a list suitable for the TMB model
#' @export
#'
#' @examples
#' df <- getParameters_ss(mod) # check
getParameters_ss <- function(mod = NA){

  #' Export EM parameters for TMB based on csv files with parameters
  #' @trueparms Use csv or stochastic parameters
  #' @mod r4ss object containing parameters



     if(all(is.na(mod))){
        stop('require SS3 model as input')
      }
          # (those with NA for Active_Cnt are not estimated)
      pars <- mod$parameters[,c("Value","Active_Cnt")]
      pars <- pars[is.na(pars$Active_Cnt) == 0,] # Remove parameters that are not estimated
      nms <- rownames(pars)

      ### Get the Rdev params
      ninit.idx <- grep('Early_Init', nms)

      years <- mod$startyr:mod$endyr
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

      parms <- list( # Just start all the simluations with the same initial conditions
        logRinit = pars$Value[which(nms == "SR_LN(R0)")],
        logh = log(pars$Value[which(nms == "SR_BH_steep")]),
        logMinit = log(pars$Value[which(nms == "NatM_p_1_Fem_GP_1")]),
        logSDsurv = log(pars$Value[which(nms == "Q_extraSD_Acoustic_Survey(2)")]),
        #logSDR = log(1.4),
        logphi_catch = pars$Value[which(nms =="ln(EffN_mult)_1")],
        #logphi_survey = log(10),
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


  return(parms)
}
